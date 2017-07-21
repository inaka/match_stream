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
  lists:foreach(
    fun(MatchPid) ->
            gen_server:cast(MatchPid, Event)
    end, match_stream_match_sup:match_pids(MatchId)).

%% @doc Returns the gen_event manager for the closest match process
-spec event_manager(match_stream:match_id()) -> atom().
event_manager(MatchId) ->
  binary_to_atom(<<"match_event_manager@",  MatchId/binary>>, utf8).

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
  EventMgr =
    case gen_event:start_link({local, event_manager(MatchId)}) of
      {ok, P} -> P;
      {error, {already_started, P}} -> P
    end,
  EventMgrRef = erlang:monitor(process, EventMgr),
  ?INFO("Match ~s initialized~n", [MatchId]),
  {ok, #state{match_id      = MatchId,
              event_mgr_ref = EventMgrRef}}.

%% @hidden
-spec handle_call(X, reference(), state()) -> {stop, {unknonw_request, X}, {unknown_request, X}, state()}.
handle_call(X, _From, State) ->
  {stop, {unknonw_request, X}, {unknown_request, X}, State}.

%% @hidden
-spec handle_cast(match_stream:event(), state()) -> {noreply, state(), hibernate} | {stop, normal, state()}.
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast(Event, State) ->
  try
    ?INFO("~p on ~s (~p clients)~n", [Event#match_stream_event.kind,
                                      Event#match_stream_event.match_id,
                                      length(gen_event:which_handlers(event_manager(Event#match_stream_event.match_id)))]),
    ok = match_stream_db:match_update(Event#match_stream_event.match_id,
                                      fun(Match) -> update_match(Match, Event) end),
    ok = match_stream_db:event_log(Event),
    ok = gen_event:notify(event_manager(State#state.match_id), Event),
    {noreply, State, hibernate}
  catch
    _:Error ->
      ?ERROR("Error processing ~p on ~s: ~p~n", [Event#match_stream_event.kind, State#state.match_id, Error]),
      {stop, Error, State}
  end.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state(), hibernate}.
handle_info({'DOWN', EventMgrRef, _Type, EventMgr, Info},
            State = #state{event_mgr_ref  = EventMgrRef}) ->
  ?WARN("Event manager for ~s crashed: ~p. Restarting...~n", [State#state.match_id, Info]),
  EventMgr =
    case gen_event:start_link({local, event_manager(State#state.match_id)}) of
      {ok, P} -> P;
      {error, {already_started, P}} -> P
    end,
  EventMgrRef = erlang:monitor(process, EventMgr),
  {noreply, State#state{event_mgr_ref = EventMgrRef}, hibernate}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, State) ->
  _ = erlang:demonitor(State#state.event_mgr_ref, [flush]),
  gen_event:stop(event_manager(State#state.match_id)).

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
                           period = first,
                           period_start = match_stream:timestamp()};
update_match(Match, #match_stream_event{kind = halftime}) ->
  Match#match_stream_match{period = halftime,
                           period_start = match_stream:timestamp()};
update_match(Match, #match_stream_event{kind = continue}) ->
  Match#match_stream_match{period = last,
                           period_start = match_stream:timestamp()};
update_match(Match, #match_stream_event{kind = penalties}) ->
  Match#match_stream_match{period = penalties,
                           period_start = match_stream:timestamp()};
update_match(Match, #match_stream_event{kind = stop}) ->
  Match#match_stream_match{period = ended,
                           period_start = undefined};
update_match(Match, #match_stream_event{kind = goal, data = Data}) ->
  case {proplists:get_value(team, Data),
        Match#match_stream_match.home#match_stream_team.team_id,
        Match#match_stream_match.visit#match_stream_team.team_id} of
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
        Match#match_stream_match.home#match_stream_team.team_id,
        Match#match_stream_match.visit#match_stream_team.team_id} of
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
        Match#match_stream_match.home#match_stream_team.team_id,
        Match#match_stream_match.visit#match_stream_team.team_id} of
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
