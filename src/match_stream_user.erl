%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Match Stream User
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_user).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("match_stream.hrl").

-record(state, {user_id       :: match_stream:user_id(),
                matches = []  :: [{pid(), match_stream:match_id(), reference(), reference()}]}).
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
  case gen_server:call(UserPid, {watch, MatchId, Client}, 20000) of
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
-spec init(match_stream:user_id()) -> {ok, state(), 0}.
init(UserId) ->
  {ok, #state{user_id = UserId}, 0}.

%% @hidden
-spec handle_call({watch, match_stream:match_id(), pid()}, reference(), state()) -> {reply, ok | {error, term()}, state(), hibernate}.
handle_call({watch, MatchId, Client}, _From, State) ->
  try
    %% First we get current status
    case match_stream_db:match(MatchId) of
      not_found ->
        {reply, {error, {not_found, MatchId}}, State, hibernate};
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
        %% or we disconnect the client if there's no manager to subscribe to
        MatchRef =
          case Match#match_stream_match.period of
            not_started ->
              match_stream_client:disconnect(Client),
              undefined;
            ended ->
              match_stream_client:disconnect(Client),
              undefined;
            _ ->
              ok = match_stream_user_handler:add_handler(MatchId, State#state.user_id, Client),
              erlang:monitor(process, erlang:whereis(match_stream_match:event_manager(MatchId)))
          end,
        ClientRef = erlang:monitor(process, Client),
        {reply, ok,
         State#state{matches = [{Client, MatchId, ClientRef, MatchRef} | State#state.matches]},
         hibernate}
    end
  catch
    _:Error ->
      ?WARN("~s couldn't subscribe to ~s: ~p~n", [State#state.user_id, MatchId, Error]),
      ok = match_stream_client:err(Client, <<"Couldn't subscribe to match.\n">>),
      {reply, {error, Error}, State, hibernate}
  end.

%% @hidden
-spec handle_cast(match_stream:event(), state()) -> {noreply, state(), hibernate}.
handle_cast(Event, State) ->
  MatchId = Event#match_stream_event.match_id,
  lists:foreach(
    fun({Client, _, _, _}) ->
            case Event#match_stream_event.kind of
              stop -> match_stream_user_handler:delete_handler(MatchId, State#state.user_id, Client);
              _ -> ok
            end,
            ok = match_stream_client:send(Client, Event)
    end,
    lists:filter(
      fun({_, M, _, _}) -> M == MatchId end,
      State#state.matches)),
  {noreply, State, hibernate}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state(), hibernate} | {stop, normal, state()}.
handle_info(timeout, State) ->
  case match_stream_db:user(State#state.user_id) of
    not_found ->
      match_stream_db:create(#match_stream_user{user_id = State#state.user_id});
    _User ->
      match_stream_db:user_update(
        State#state.user_id,
        fun(#match_stream_user{visit_count = V} = User) ->
                User#match_stream_user{visit_count = V + 1}
        end)
  end,
  {noreply, State, hibernate};
handle_info({'DOWN', Ref, _Type, Pid, _Info}, State) ->
  case lists:keytake(Ref, 3, State#state.matches) of
    {value, {Pid, MatchId, Ref, MatchRef}, OtherMatches} ->
      try
        _ = match_stream_user_handler:delete_handler(MatchId, State#state.user_id, Pid),
        _ = erlang:demonitor(MatchRef, [flush])
      catch
        _:Error ->
          ?WARN("~s couldn't unsubscribe to ~s: ~p~n", [State#state.user_id, MatchId, Error])
      end,
      case OtherMatches of
        [] -> %% It was the last match
          {stop, normal, State#state{matches = []}};
        OtherMatches ->
          {noreply, State#state{matches = OtherMatches}, hibernate}
      end;
    false ->
      case lists:keytake(Ref, 4, State#state.matches) of
        {value, {Client, MatchId, ClientRef, Ref}, OtherMatches} ->
          ?WARN("Match ~s event manager down~n", [MatchId]),
          _ = erlang:demonitor(ClientRef, [flush]),
          ok = match_stream_client:disconnect(Client),
          case OtherMatches of
            [] -> %% It was the last match
              {stop, normal, State#state{matches = []}};
            OtherMatches ->
              {noreply, State#state{matches = OtherMatches}, hibernate}
          end;
        false ->
          {noreply, State, hibernate}
      end
  end.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, #state{matches = Matches, user_id = Uid}) ->
  lists:foreach(
    fun({Client, MatchId, _, _}) ->
            catch match_stream_client:disconnect(Client),
            catch match_stream_user_handler:delete_handler(MatchId, Uid, Client)
    end, Matches).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
