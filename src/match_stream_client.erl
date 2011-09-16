%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Labs S.R.L.
%%% @doc Client Process
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_client).
-author('Fernando Benavides <fbenavides@novamens.com>').

-include("match_stream.hrl").

-behaviour(gen_fsm).

-export([start_link/0, set_socket/2]).
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).
-export([wait_for_socket/2, wait_for_params/2, running/2]).
-export([send/2, disconnect/1]).

-define(FSM_TIMEOUT, 60000).

-record(state, {socket        :: port(),
                buffer = <<>> :: binary(),
                peerport      :: integer()}).

%% ====================================================================
%% External functions
%% ====================================================================
%% -- General ---------------------------------------------------------
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_fsm:start_link(?MODULE, [], []).

%% @hidden
-spec set_socket(pid(), port()) -> ok.
set_socket(Client, Socket) ->
  gen_fsm:send_event(Client, {socket_ready, Socket}).

%% @doc Notifies an event through the socket
-spec send(pid(), term()) -> ok.
send(Client, Event) ->
  gen_fsm:send_event(Client, {send, Event}).

%% @doc disconnects the client
-spec disconnect(pid()) -> ok.
disconnect(Client) ->
  gen_fsm:send_event(Client, disconnect).

%% ====================================================================
%% FSM functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, wait_for_socket, #state{}, ?FSM_TIMEOUT}.
init([]) ->
  {ok, wait_for_socket, #state{}, ?FSM_TIMEOUT}.

%% ASYNC EVENTS -------------------------------------------------------
%% @hidden
-spec wait_for_socket({socket_ready, port()} | timeout | term(), #state{}) -> {next_state, wait_for_socket, #state{}, ?FSM_TIMEOUT} | {stop, timeout, #state{}}.
wait_for_socket({socket_ready, Socket}, State) ->
  % Now we own the socket
  PeerPort =
    case inet:peername(Socket) of
      {ok, {_Ip, Port}} -> Port;
      Error -> Error
    end,
  error_logger:info_msg("~p connected~n", [PeerPort]),
  ok = inet:setopts(Socket, [{active, once}, {packet, 0}, binary]),
  {next_state, wait_for_params, State#state{socket   = Socket,
                                            peerport = PeerPort}};
wait_for_socket(timeout, State) ->
  {stop, timeout, State};
wait_for_socket(Other, State) ->
  error_logger:warning_msg("Unexpected message: ~p\n", [Other]),
  {next_state, wait_for_socket, State, ?FSM_TIMEOUT}.

%% @hidden
-spec wait_for_params(term(), #state{}) -> {next_state, running, #state{}} | {stop, timeout, #state{}} | {stop, {unexpected_event, term()}, #state{}}.
wait_for_params({data, Payload}, State = #state{peerport = PeerPort}) ->
  error_logger:info_msg("Data received:~p~n",[Payload]),
  case binary:split(Payload, [<<":">>, <<$\r>>, <<$\n>>], [global]) of
    [<<"VERSION">>, <<"1">>, <<"CONNECT">>, Uid, <<"MATCH">>, MatchId | _] ->
      try
        ok = match_stream_user:watch(Uid, MatchId, self())
      catch
        Type:Error ->
          ok = tcp_send(State#state.socket, frame(io_lib:format("ERROR: ~p~n", [Error])), State),
          throw({stop, {error, Type, Error}, State})
      end,
      error_logger:info_msg("~s watching ~s using ~p ~n", [Uid, MatchId, PeerPort]);
    _ -> 
      error_logger:warning_msg("unknown socket command ~p~n",[Payload]),
      throw({stop, {unknown_client_command, Payload}, State})
  end,
  {next_state, running, State};
wait_for_params(timeout, State) ->
    {stop, timeout, State};
wait_for_params(Event, State) ->
    error_logger:warning_msg("Unexpected Event: ~p~n", [Event]),
    {stop, {unexpected_event, Event}, State}.

%% @hidden
-spec running(term(), #state{}) -> {next_state, running, #state{}} | {stop, normal | {unexpected_event, term()}, #state{}}.
running({send, Message}, State = #state{socket = S}) ->
  error_logger:info_msg("Sending: ~p~n", [Message]),
  ok = tcp_send(S, frame(Message), State),
  {next_state, running, State};
running(disconnect, State) ->
  error_logger:info_msg("disconnecting...~n", []),
  {stop, normal, State};
running(Event, State) ->
  error_logger:warning_msg("Unexpected Event:~n\t~p~n", [Event]),
  {stop, {unexpected_event, Event}, State}.

%% OTHER EVENTS -------------------------------------------------------
%% @hidden
-spec handle_event(X, atom(), #state{}) -> {stop, {atom(), undefined_event, X}, #state{}}.
handle_event(Event, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%% @hidden
-spec handle_sync_event(X, reference(), atom(), #state{}) -> {stop, {atom(), undefined_event, X}, #state{}}.
handle_sync_event(Event, _From, StateName, StateData) ->
    {stop, {StateName, undefined_event, Event}, StateData}.

%% @hidden
-spec handle_info(term(), atom(), #state{}) -> term().
handle_info({tcp, Socket, Bin}, StateName, #state{socket = Socket} = StateData) ->
    % Flow control: enable forwarding of next TCP message
    ok = inet:setopts(Socket, [{active, false}]),
    Result = ?MODULE:StateName({data, Bin}, StateData),
    ok = inet:setopts(Socket, [{active, once}]),
    Result;
handle_info({tcp_closed, Socket}, _StateName, #state{socket = Socket,
                                                     peerport = PeerPort} = StateData) ->
    error_logger:info_msg("Disconnected ~p.~n", [PeerPort]),
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

%% @hidden
-spec terminate(term(), atom(), #state{}) -> ok.
terminate(normal, _StateName, #state{socket = Socket}) ->
  (catch gen_tcp:close(Socket)),
  ok;
terminate(Reason, _StateName, #state{socket = Socket}) ->
  error_logger:info_msg("Terminating client: ~p~n", [Reason]),
  (catch gen_tcp:close(Socket)),
  ok.

%% @hidden
-spec code_change(term(), atom(), #state{}, any()) -> {ok, atom(), #state{}}.
code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% @doc  Sends a message through TCP socket or fails gracefully 
%%       (in a gen_fsm fashion)
-spec tcp_send(port(), iolist(), #state{}) -> ok.
tcp_send(Socket, Message, State) ->
  try gen_tcp:send(Socket, Message) of
    ok ->
      ok;
    {error, closed} ->
      error_logger:info_msg("Connection closed~n", []),
      throw({stop, normal, State});
    {error, timeout} ->
      error_logger:info_msg("Connection automatically closed due to send_timeout~n", []),
      throw({stop, normal, State});
    {error, Error} ->
      error_logger:warning_msg("Couldn't send msg through TCP~n\tError: ~p~n", [Error]),
      throw({stop, {error, Error}, State})
  catch
    _:{Exception, _} ->
      error_logger:warning_msg("Couldn't send msg through TCP~n\tError: ~p~n", [Exception]),
      throw({stop, normal, State});
    _:Exception ->
      error_logger:warning_msg("Couldn't send msg through TCP~n\tError: ~p~n", [Exception]),
      throw({stop, normal, State})
  end.

frame(#match_stream_event{timestamp = TS, kind = Kind, data = Data}) ->
  frame(
    [io_lib:format("~s: ~p:~n", [dtformat(TS), Kind]) |
     lists:map(fun({K, Players}) when K == home_players; K == visit_players ->
                       V =
                         case Players of
                           undefined -> "";
                           [] -> "";
                           Players ->
                             lists:map(
                               fun({B, M}) ->
                                       io_lib:format("\t\t ~s (~p) ~n", [M,B])
                               end, Players)
                         end,
                       io_lib:format("\t~p:~n~s", [K,V]);
                  ({K, {B, M}}) ->
                       io_lib:format("\t~p: ~s (~p) ~n", [K,M,B]);
                  ({K, V}) ->
                       io_lib:format("\t~p: ~p~n", [K,V])
               end, Data)]);
frame(Msg) -> 
  Result = [Msg, "\r\n"],
  error_logger:info_msg("To client: ~s~n", [Result]),
  Result.

dtformat(TS) ->
  {{Y, M, D}, {H, N, S}} = calendar:gregorian_seconds_to_datetime(erlang:round(TS/1000)),
  [lpad(Y), $-, lpad(M), $-, lpad(D), $\s, lpad(H), $:, lpad(N), $:, lpad(S)].

lpad(X) when X < 10 -> [$0 | integer_to_list(X)];
lpad(X) -> integer_to_list(X).