%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
%%% @doc Match Stream User
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_user).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("match_stream.hrl").

-record(state, {user_id       :: match_stream:user_id(),
                matches = []  :: [{pid(), match_stream:match_id(), reference()}]}).
-opaque state() :: #state{}.

-export([watch/3]).
-export([start_link/1, notify/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @doc User wants to watch Match through Client
-spec watch(match_stream:user_id(), match_stream:match_id(), pid()) -> ok.
watch(UserId, MatchId, Client) ->
  UserPid =
    case match_stream_user_sup:start_user(UserId) of
      {ok, Pid} -> Pid;
      {error, {already_started, Pid}} -> Pid
    end,
  case gen_server:call(UserPid, {watch, MatchId, Client}) of
    ok -> ok;
    {error, Error} -> throw({error, Error})
  end.

%% =================================================================================================
%% Internal (i.e. used only by other modules) functions
%% =================================================================================================
%% @hidden
-spec notify(pid(), match_stream:event()) -> ok.
notify(UserPid, Event) ->
  gen_server:cast(UserPid, Event).

%% @hidden
-spec start_link(match_stream:user_id()) -> {ok, pid()}.
start_link(UserId) ->
  gen_server:start_link(?MODULE, UserId, []).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init(match_stream:user_id()) -> {ok, state()}.
init(UserId) -> {ok, #state{user_id = UserId}}.

%% @hidden
-spec handle_call({watch, match_stream:match_id(), pid()}, reference(), state()) -> {reply, ok | {error, term()}, state()}.
handle_call({watch, MatchId, Client}, _From, State) ->
  try
    %% First we get current status
    case match_stream_db:get(MatchId) of
      not_found ->
        {reply, {error, {not_found, MatchId}}, State};
      Match ->
        MatchStatus =
          #match_stream_event{timestamp = match_stream:timestamp(),
                              match_id  = Match#match_stream_match.match_id,
                              kind      = status,
                              data      =
                                [{home,           Match#match_stream_match.home},
                                 {home_players,   Match#match_stream_match.home_players},
                                 {home_score,     Match#match_stream_match.home_score},
                                 {visit,          Match#match_stream_match.visit},
                                 {visit_players,  Match#match_stream_match.visit_players},
                                 {visit_score,    Match#match_stream_match.visit_score},
                                 {period,         Match#match_stream_match.period}]},
        ok = match_stream_client:send(Client, MatchStatus),
        %% Then we subscribe to the match stream so we can get updates from now on
        case Match#match_stream_match.period of
          not_started -> match_stream_client:disconnect(Client);
          ended -> match_stream_client:disconnect(Client);
          _ ->
            match_stream_user_handler:add_handler(MatchId, State#state.user_id, Client)
        end,
        ClientRef = erlang:monitor(process, Client),
        {reply, ok, State#state{matches = [{Client, MatchId, ClientRef} | State#state.matches]}}
    end
  catch
    _:Error ->
      error_logger:warning_msg("~s couldn't subscribe to ~s: ~p~n", [State#state.user_id, MatchId, Error]),
      ok = match_stream_client:send(Client, "ERROR: Couldn't subscribe to match.\n"),
      {reply, {error, Error}, State}
  end.

%% @hidden
-spec handle_cast(match_stream:event(), state()) -> {noreply, state()}.
handle_cast(Event, State) ->
  MatchId = Event#match_stream_event.match_id,
  lists:foreach(
    fun({Client, _, _}) ->
            case Event#match_stream_event.kind of
              stop -> match_stream_user_handler:delete_handler(MatchId, State#state.user_id, Client);
              _ -> ok
            end,
            ok = match_stream_client:send(Client, Event)
    end,
    lists:filter(
      fun({_, M, _}) -> M == MatchId end,
      State#state.matches)),
  {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_info({'gen_event_EXIT', {match_stream_user_handler, {MatchId, UserId, Client}}, _Reason},
            State = #state{user_id = UserId}) ->
  case lists:partition(
         fun({C, M, _}) -> M == MatchId andalso C == Client end,
         State#state.matches) of
    {[], _} ->
      {noreply, State};
    {[{Client, MatchId, ClientRef}], OtherMatches} ->
      _ = erlang:demonitor(ClientRef, [flush]),
      ok = match_stream_client:disconnect(Client),
      case OtherMatches of
        [] -> %% It was the last match
          {stop, normal, State#state{matches = []}};
        OtherMatches ->
          {noreply, State#state{matches = OtherMatches}}
      end
  end;
handle_info({'DOWN', ClientRef, _Type, Client, _Info}, State) ->
  case lists:keytake(ClientRef, 3, State#state.matches) of
    {value, {Client, MatchId, ClientRef}, OtherMatches} ->
      try
        match_stream_user_handler:delete_handler(MatchId, State#state.user_id, Client)
      catch
        _:Error ->
          error_logger:warning_msg("~s couldn't unsubscribe to ~s: ~p~n", [State#state.user_id, MatchId, Error])
      end,
      case OtherMatches of
        [] -> %% It was the last match
          {stop, normal, State#state{matches = []}};
        OtherMatches ->
          {noreply, State#state{matches = OtherMatches}}
      end;
    false ->
      {noreply, State}
  end.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, #state{matches = Matches}) ->
  lists:foreach(fun({C, _, _}) -> match_stream_client:disconnect(C) end, Matches).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.