%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
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
-export([start_link/1]).
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

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init(match_stream:match_id()) -> {ok, state()}.
init(MatchId) ->
  {ok, EventMgr} = gen_event:start_link(),
  EventMgrRef = erlang:monitor(process, EventMgr),
  {ok, #state{match_id      = MatchId,
              event_manager = EventMgr,
              event_mgr_ref = EventMgrRef}}.

%% @hidden
-spec handle_call(event_manager, reference(), state()) -> {reply, pid(), state()}.
handle_call(event_manager, _From, State) ->
  {reply, State#state.event_manager, State}.

%% @hidden
-spec handle_cast(match_stream:event(), state()) -> {noreply, state()}.
handle_cast(Event, State) ->
  ok = match_stream_db:update(Event#match_stream_event.match_id,
                              fun(Match) -> update_match(Match, Event) end),
  ok = match_stream_db:log(Event),
  ok = gen_event:notify(State#state.event_manager, Event),
  {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', EventMgrRef, _Type, EventMgr, Info},
            State = #state{event_manager  = EventMgr,
                           event_mgr_ref  = EventMgrRef}) ->
  error_logger:warning_msg("Event manager for ~s crashed: ~p. Restarting...~n", [State#state.match_id, Info]),
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
  Match#match_stream_match{home_formation =
                             proplists:get_value(
                               home_formation, Data, Match#match_stream_match.home_formation),
                           home_score = 0,
                           visit_formation =
                             proplists:get_value(
                               visit_formation, Data, Match#match_stream_match.visit_formation),
                           visit_score = 0};
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
  case {proplists:get_value(card, Data), proplists:get_value(player, Data),
        Match#match_stream_match.home, Match#match_stream_match.visit} of
    {yellow, _, _, _} -> Match;
    {red, Out = #match_stream_player{team = Team, number = Number}, Team, _} ->
      Match#match_stream_match{home_formation =
                                 case lists:keytake(Number, #match_stream_player.number,
                                                    Match#match_stream_match.home_formation) of
                                   {value, Out, Rest} -> Rest;
                                   _ -> throw({invalid_red_card, {Out, Match}})
                                 end};
    {red, Out = #match_stream_player{team = Team, number = Number}, _, Team} ->
      Match#match_stream_match{visit_formation =
                                 case lists:keytake(Number, #match_stream_player.number,
                                                    Match#match_stream_match.visit_formation) of
                                   {value, Out, Rest} -> Rest;
                                   _ -> throw({invalid_red_card, {Out, Match}})
                                 end};
    {red, Out, Home, Visit} ->
      throw({invalid_team, {Out, Home, Visit}});
    {Card, Out, Home, Visit} ->
      throw({invalid_card, {Card, Out, Home, Visit}})
  end;
update_match(Match, #match_stream_event{kind = substitution, data = Data}) ->
  case {proplists:get_value(player_in, Data), proplists:get_value(player_out, Data),
        Match#match_stream_match.home, Match#match_stream_match.visit} of
    {In = #match_stream_player{team = Team},
     Out = #match_stream_player{team = Team,
                               number = Number}, Team, _} ->
      Match#match_stream_match{home_formation =
                                 case lists:keytake(Number, #match_stream_player.number,
                                                    Match#match_stream_match.home_formation) of
                                   {value, Out, Rest} ->
                                     [In|Rest]; %%TODO: Place it where it belongs
                                   _ ->
                                     throw({invalid_substitution, {In, Out, Match}})
                                 end};
    {In = #match_stream_player{team = Team},
     Out = #match_stream_player{team = Team,
                               number = Number}, _, Team} ->
      Match#match_stream_match{visit_formation =
                                 case lists:keytake(Number, #match_stream_player.number,
                                                    Match#match_stream_match.visit_formation) of
                                   {value, Out, Rest} ->
                                     [In|Rest]; %%TODO: Place it where it belongs
                                   _ ->
                                     throw({invalid_substitution, {In, Out, Match}})
                                 end};
    {In, Out, Home, Visit} ->
      throw({invalid_team, {In, Out, Home, Visit}})
  end;
update_match(Match, _OtherEvent) ->
  Match.
