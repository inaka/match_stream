%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 Inaka Networks S.R.L.
%%% @doc Pobox event handler
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_user_handler).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-include("match_stream.hrl").

-behaviour(gen_event).

-export([add_handler/3, delete_handler/3]).
-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {user_pid :: pid()}).
-type state() :: #state{}.

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc Subscribes user to match.
-spec add_handler(match_stream:match_id(), match_stream:user_id(), pid()) -> ok.
add_handler(MatchId, UserId, Client) ->
  gen_event:add_sup_handler(
    match_stream_match:event_manager(MatchId),
    {?MODULE, {MatchId, UserId, Client}}, self()).

%% @doc Unsubscribes user to match
-spec delete_handler(match_stream:match_id(), match_stream:user_id(), pid()) -> ok.
delete_handler(MatchId, UserId, Client) ->
  gen_event:delete_handler(
    match_stream_match:event_manager(MatchId),
    {match_stream_user_handler, {MatchId, UserId, Client}}, normal).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init(pid()) -> {ok, state()}.
init(UserPid) ->
  {ok, #state{user_pid = UserPid}}.

%% @hidden
-spec handle_event(term(), state()) -> {ok, state()}.
handle_event(Event, State = #state{user_pid = UserPid}) ->
  match_stream_user:notify(UserPid, Event),
  {ok, State}.

%% @hidden
-spec handle_call(term(), state()) -> {ok, ok, state()}.
handle_call(_Request, State) -> {ok, ok, State}.
%% @hidden
-spec handle_info(term(), state()) -> {ok, state()}.
handle_info({'EXIT', Pid, normal}, State) ->
  Name = lists:keyfind(registered_name, 1, erlang:process_info(self(), [registered_name])),
  ?WARN("Linked process ~p terminated~nState: ~p~nRunning on: ~p~n", [Pid, State, Name]),
  {ok, State};
handle_info(Info, State) ->
  ?WARN("Unexpected Info:~n\t~p~n", [Info]),
  {ok, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(normal, _State) ->
  ok;
terminate(stop, #state{user_pid = UserPid}) ->
  ?WARN("event manager terminating... ~p should be informed~n", [UserPid]);
terminate({stop, Reason}, #state{user_pid = UserPid}) ->
  ?WARN("~p terminated with reason: ~p~n", [UserPid, Reason]);
terminate(Reason, #state{user_pid = UserPid}) ->
  error_logger:error_msg("~p terminated with reason: ~p~n", [UserPid, Reason]).

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
