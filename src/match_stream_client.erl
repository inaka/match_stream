%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Labs S.R.L.
%%% @doc Client Process
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_client).
-author('Fernando Benavides <fbenavides@novamens.com>').

-include("match_stream.hrl").
-include("socketio.hrl").
-include("misultin.hrl").

-behaviour(gen_event).

-export([start/1]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).
-export([send/2, err/2, disconnect/1]).

-record(state, {connected = false :: boolean()}).
-type state() :: #state{}.

%% ====================================================================
%% External functions
%% ====================================================================
%% -- General ---------------------------------------------------------
%% @hidden
-spec start(pid()) -> ok.
start(ClientPid) ->
  case application:get_env(socketio_connection_timeout) of
    undefined -> ok;
    infinity -> ok;
    ConnectionTimeout ->
      erlang:send_after(ConnectionTimeout,
                        socketio_client:event_manager(ClientPid),
                        {connection_timeout, ClientPid}),
      ok
  end,
  case socketio_client:request(ClientPid) of
    {misultin_req, #req{peer_addr = Peer, uri = {abs_path, Uri}}, _Pid} ->
      ?INFO("New socket.io client: From ~s using ~s~n", [inet_parse:ntoa(Peer), Uri]);
    {misultin_ws, #ws{peer_addr = Peer, path = Uri}, _Pid} ->
      ?INFO("New socket.io client: From ~s using ~s~n", [inet_parse:ntoa(Peer), Uri]);
    Other ->
      ?INFO("New socket.io client: using~n\t~p~n", [Other])
  end,
  gen_event:add_handler(socketio_client:event_manager(ClientPid), ?MODULE, []).

%% @doc Notifies an event to the client
-spec send(pid(), #match_stream_event{}) -> ok.
send(Client, Event) ->
  ?DEBUG("socketio client (~p) sending ~p~n", [Client, Event]),
  EncodedMessage = #msg{content = to_json(Event), json = true},
  case rpc:pinfo(Client) of
    undefined -> throw({dead_client, Client});
    _ -> socketio_client:send(Client, EncodedMessage)
  end.

%% @doc Notifies an error to the client
-spec err(pid(), binary()) -> ok.
err(Client, Error) ->
  ?DEBUG("socketio client (~p) err'ing ~s~n", [Client, Error]),
  EncodedMessage = #msg{content = [{<<"error">>, <<"true">>},{<<"desc">>, Error}], json = true},
  case rpc:pinfo(Client) of
    undefined -> throw({dead_client, Client});
    _ -> socketio_client:send(Client, EncodedMessage)
  end.

%% @doc disconnects the client
-spec disconnect(pid()) -> ok.
disconnect(Client) ->
  catch socketio_client:stop(Client),
  ok.

%% ====================================================================
%% FSM functions
%% ====================================================================
%% @hidden
-spec init(reference()) -> {ok, state()}.
init([]) -> {ok, #state{}}.

%% @hidden
-spec handle_event(#msg{}, state()) -> {ok, state()}.
handle_event({message, ClientPid, #msg{json = true, content = MsgProps}}, State) ->
  case to_lower(proplists:get_value(<<"command">>, MsgProps)) of
    undefined -> err(ClientPid, <<"Missing parameter 'command'">>);
    Command -> ok = handle_command(Command, ClientPid, MsgProps)
  end,
  {ok, State#state{connected = true}};
handle_event({message, ClientPid, #msg{json = false, content = Command}}, State) ->
  ?WARN("unknown socket command ~p in~n\t~p~n", [Command, sys:get_status(ClientPid)]),
  err(ClientPid, <<"Unknown socket message: ", Command/binary>>),
  {ok, State};
handle_event(Event, State) ->
  ?INFO("Ignored socketio event: ~p~n", [Event]),
  {ok, State}.

%% @hidden
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.

%% @hidden
-spec handle_info({connection_timeout, pid()} | term(), state()) -> remove_handler | {ok, state()}.
handle_info({connection_timeout, ClientPid}, #state{connected = false}) ->
  ?WARN("Somebody didn't tune in fast enough:~n~p~n", [sys:get_status(ClientPid)]),
  catch socketio_client:stop(ClientPid),
  remove_handler;
handle_info(_Info, State) -> {ok, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% ====================================================================
%% Private functions
%% ====================================================================
to_json(#match_stream_event{timestamp = TS, kind = Kind, data = Data}) ->
  [{<<"timestamp">>, TS},
   {<<"kind">>, atom_to_binary(Kind, utf8)} |
       lists:map(fun to_json/1, Data)];
to_json({Key, Players}) when Key == home_players;
                             Key == visit_players ->
  case Players of
    undefined -> {atom_to_binary(Key, utf8), null};
    Players -> {atom_to_binary(Key, utf8), Players}
  end;
to_json({Key, {B,M}}) ->
  {atom_to_binary(Key, utf8), [{B, M}]};
to_json({Key, Value}) when is_number(Value);
                           is_binary(Value);
                           Value == true;
                           Value == false;
                           Value == null ->
  {atom_to_binary(Key, utf8), Value};
to_json({Key, Value}) ->
  {atom_to_binary(Key, utf8),
   erlang:iolist_to_binary(io_lib:format("~p", [Value]))}.

to_lower(Bin) when is_binary(Bin) ->
  list_to_binary(string:to_lower(binary_to_list(Bin)));
to_lower(_Other) ->
  undefined.

handle_command(<<"watch">>, ClientPid, MsgProps) ->
  case {proplists:get_value(<<"uid">>, MsgProps, null),
        proplists:get_value(<<"mid">>, MsgProps, null)} of
    {null, _} -> err(ClientPid, <<"Missing parameter 'uid'">>);
    {_, null} -> err(ClientPid, <<"Missing parameter 'mid'">>);
    {Uid, MatchId} ->
      try
        ok = match_stream_user:watch(Uid, MatchId, ClientPid)
      catch
        _:{error, {not_found, MatchId}} ->
          err(ClientPid, <<"Match not found: ", MatchId/binary>>);
        _:{error, Error} ->
          ?WARN("Watch Error: ~p~n", [Error]),
          err(ClientPid, <<"Error trying to watch ", MatchId/binary>>)
      end
  end;
handle_command(Command, ClientPid, _MsgProps) ->
  err(ClientPid, <<"Unsupported command '", Command/binary, "'">>).