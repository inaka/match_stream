%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Match Stream Match
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_match).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("match_stream.hrl").

-record(state, {match_id      :: match_stream:match_id(),
                event_manager :: pid(),
                event_mgr_ref :: reference()}).
-opaque state() :: #state{}.

-export([apply/1, event_manager/1]).
-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @doc Registers an event in the match
-spec apply(match_stream:event()) -> ok.
apply(Event = #match_stream_event{match_id = MatchId}) ->
  MatchPid =
    case match_stream_match_sup:start_match(MatchId) of
      {ok, Pid} -> Pid;
      {error, {already_started, Pid}} -> Pid
    end,
  gen_server:cast(MatchPid, Event).

%% @doc Returns the gen_event manager for the match
-spec event_manager(match_stream:match_id()) -> pid().
event_manager(MatchId) ->
  case gen_server:call(process(MatchId), event_manager) of
    Pid when is_pid(Pid) -> Pid;
    Error -> throw(Error)
  end.

%% =================================================================================================
%% Internal (i.e. used only by other modules) functions
%% =================================================================================================
%% @hidden
-spec start_link(match_stream:match_id()) -> {ok, pid()}.
start_link(MatchId) ->
  gen_server:start_link({local, process(MatchId)}, ?MODULE, MatchId, []).

%% @doc Stops the match. If it's already stopped it does nothing.
-spec stop(match_stream:match_id()) -> ok.
stop(MatchId) ->
  gen_server:cast(process(MatchId), stop).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init(match_stream:match_id()) -> {ok, state()}.
init(MatchId) ->
  {ok, EventMgr} = gen_event:start_link(),
  EventMgrRef = erlang:monitor(process, EventMgr),
  ?INFO("Match ~s initialized~n", [MatchId]),
  {ok, #state{match_id      = MatchId,
              event_manager = EventMgr,
              event_mgr_ref = EventMgrRef}}.

%% @hidden
-spec handle_call(event_manager, reference(), state()) -> {reply, pid(), state()}.
handle_call(event_manager, _From, State) ->
  {reply, State#state.event_manager, State}.

%% @hidden
-spec handle_cast(match_stream:event(), state()) -> {noreply, state()} | {stop, normal, state()}.
handle_cast(stop, State) ->
  _ = erlang:demonitor(State#state.event_mgr_ref, [flush]),
  ok = gen_event:stop(State#state.event_manager),
  {stop, normal, State};
handle_cast(Event, State) ->
  try
    ok = match_stream_db:update(Event#match_stream_event.match_id,
                                fun(Match) -> update_match(Match, Event) end),
    ok = match_stream_db:log(Event),
    ok = gen_event:notify(State#state.event_manager, Event),
    {noreply, State}
  catch
    _:Error ->
      {stop, Error, State}
  end.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', EventMgrRef, _Type, EventMgr, Info},
            State = #state{event_manager  = EventMgr,
                           event_mgr_ref  = EventMgrRef}) ->
  ?WARN("Event manager for ~s crashed: ~p. Restarting...~n", [State#state.match_id, Info]),
  EventMgrRef = erlang:monitor(process, EventMgr),
  {noreply, State#state{event_manager = EventMgr,
                        event_mgr_ref = EventMgrRef}}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =================================================================================================
%% Private functions
%% =================================================================================================
-spec process(match_stream:match_id()) -> atom().
process(MatchId) ->
  binary_to_atom(MatchId, utf8).

-spec update_match(match_stream:match(), match_stream:event()) -> match_stream:match().
update_match(Match, #match_stream_event{kind = start, data = Data}) ->
  Match#match_stream_match{home_players =
                             proplists:get_value(
                               home_players, Data, Match#match_stream_match.home_players),
                           home_score = 0,
                           visit_players =
                             proplists:get_value(
                               visit_players, Data, Match#match_stream_match.visit_players),
                           visit_score = 0,
                           period = first};
update_match(Match, #match_stream_event{kind = halftime_start}) ->
  Match#match_stream_match{period =
                             case Match#match_stream_match.period of
                               first -> halftime;
                               first_extra -> halftime_extra;
                               Period -> Period
                             end};
update_match(Match, #match_stream_event{kind = halftime_stop}) ->
  Match#match_stream_match{period =
                             case Match#match_stream_match.period of
                               halftime -> last;
                               halftime_extra -> last_extra;
                               Period -> Period
                             end};
update_match(Match, #match_stream_event{kind = stop}) ->
  Match#match_stream_match{period = ended};
update_match(Match, #match_stream_event{kind = extratime}) ->
  Match#match_stream_match{period = first_extra};
update_match(Match, #match_stream_event{kind = goal, data = Data}) ->
  case {proplists:get_value(team, Data), Match#match_stream_match.home, Match#match_stream_match.visit} of
    {Team, Team, _} ->
      Match#match_stream_match{home_score = Match#match_stream_match.home_score + 1};
    {Team, _, Team} ->
      Match#match_stream_match{visit_score = Match#match_stream_match.visit_score + 1};
    {Team, Home, Visit} ->
      throw({invalid_team, {Team, Home, Visit}})
  end;
update_match(Match, #match_stream_event{kind = card, data = Data}) ->
  case {proplists:get_value(card, Data),
        proplists:get_value(player, Data),
        proplists:get_value(team, Data),
        Match#match_stream_match.home, Match#match_stream_match.visit} of
    {yellow, _, _, _, _} -> Match;
    {red, Out = {Number, _Name}, Team, Team, _} ->
      Match#match_stream_match{home_players =
                                 case lists:keytake(Number, 1,
                                                    Match#match_stream_match.home_players) of
                                   {value, Out, Rest} -> Rest;
                                   _ -> throw({invalid_red_card, {Out, Match}})
                                 end};
    {red, Out = {Number, _Name}, Team, _, Team} ->
      Match#match_stream_match{visit_players =
                                 case lists:keytake(Number, 1,
                                                    Match#match_stream_match.visit_players) of
                                   {value, Out, Rest} -> Rest;
                                   _ -> throw({invalid_red_card, {Out, Match}})
                                 end};
    {red, Out, Team, Home, Visit} ->
      throw({invalid_team, {Out, Team, Home, Visit}});
    {Card, Out, Team, Home, Visit} ->
      throw({invalid_card, {Card, Out, Team, Home, Visit}})
  end;
update_match(Match, #match_stream_event{kind = substitution, data = Data}) ->
  case {proplists:get_value(player_in, Data),
        proplists:get_value(player_out, Data),
        proplists:get_value(team, Data),
        Match#match_stream_match.home, Match#match_stream_match.visit} of
    {In, Out = {Number, _Name}, Team, Team, _} ->
      Match#match_stream_match{home_players =
                                 case lists:keytake(Number, 1,
                                                    Match#match_stream_match.home_players) of
                                   {value, Out, Rest} ->
                                     [In|Rest]; %%TODO: Place it where it belongs
                                   _ ->
                                     throw({invalid_substitution, {In, Out, Match}})
                                 end};
    {In, Out = {Number, _}, Team, _, Team} ->
      Match#match_stream_match{visit_players =
                                 case lists:keytake(Number, 1,
                                                    Match#match_stream_match.visit_players) of
                                   {value, Out, Rest} ->
                                     [In|Rest]; %%TODO: Place it where it belongs
                                   _ ->
                                     throw({invalid_substitution, {In, Out, Match}})
                                 end};
    {In, Out, Team, Home, Visit} ->
      throw({invalid_team, {In, Out, Team, Home, Visit}})
  end;
update_match(Match, _OtherEvent) ->
  Match.
