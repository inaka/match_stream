%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Match Stream Database main interface.
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_db).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(supervisor).

-include("match_stream.hrl").

-export([create/1, match_update/2, user_update/2, match_all/0, user_all/0, match/1, user/1,
         match_delete/1, user_delete/1, event_log/1, match_history/1]).
-export([start_link/0, init/1]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @doc Creates a match or an user
%% @throws duplicated
-spec create(match_stream:match() | match_stream:user()) -> ok.
create(Object = #match_stream_match{match_id = MatchId}) ->
  make_call(write, {create, <<"match-", MatchId/binary>>, Object});
create(Object = #match_stream_user{user_id = UserId}) ->
  make_call(write, {create, <<"user-", UserId/binary>>, Object}).

%% @doc Updates an existing match
%% @throws not_found
-spec match_update(match_stream:match_id(), fun((match_stream:match()) -> match_stream:match())) -> ok.
match_update(MatchId, UpdateFun) ->
  make_call(write_once, {update, <<"match-", MatchId/binary>>, UpdateFun}).

%% @doc Updates an existing user
%% @throws not_found
-spec user_update(match_stream:user_id(), fun((match_stream:user()) -> match_stream:user())) -> ok.
user_update(UserId, UpdateFun) ->
  make_call(write, {update, <<"user-", UserId/binary>>, UpdateFun}).

%% @doc Returns the list of available matches
-spec match_all() -> [match_stream:match_id()].
match_all() ->
  make_call(read, {all, "match-*"}).

%% @doc Returns the list of available users
-spec user_all() -> [match_stream:user_id()].
user_all() ->
  make_call(read, {all, "user-*"}).

%% @doc Returns a match
-spec match(match_stream:match_id()) -> match_stream:match() | not_found.
match(MatchId) ->
  make_call(read, {get, <<"match-", MatchId/binary>>}).

%% @doc Returns an user
-spec user(match_stream:user_id()) -> match_stream:user() | not_found.
user(UserId) ->
  make_call(read, {get, <<"user-", UserId/binary>>}).

%% @doc Deletes a match.
%% Silently ignores the call if the match is not there
-spec match_delete(match_stream:match_id()) -> ok.
match_delete(MatchId) ->
  make_call(write_once, {delete, <<"match-", MatchId/binary>>}),
  make_call(write_once, {delete_events, MatchId}).

%% @doc Deletes an user.
%% Silently ignores the call if the user is not there
-spec user_delete(match_stream:user_id()) -> ok.
user_delete(UserId) ->
  make_call(write, {delete, <<"user-", UserId/binary>>}).

%% @doc Logs an event
%% @throws not_found
-spec event_log(match_stream:event()) -> ok.
event_log(Event) ->
  make_call(write_once, {log, Event}).

%% @doc Returns match events
-spec match_history(match_stream:match_id()) -> [match_stream:event()].
match_history(MatchId) ->
  make_call(read, {match_history, MatchId}).

%% =================================================================================================
%% Internal (i.e. used only by other modules) functions
%% =================================================================================================
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Reader = {match_stream_db_reader, {match_stream_db_reader, start_link, []},
            permanent, brutal_kill, worker, [match_stream_db_reader]},
  Writer = {match_stream_db_writer, {match_stream_db_writer, start_link, []},
            permanent, brutal_kill, worker, [match_stream_db_writer]},
  ?INFO("Main DB supervisor initialized~n", []),
  {ok, {{one_for_one, 5, 10}, [Reader, Writer]}}.

%% =================================================================================================
%% Private functions
%% =================================================================================================
make_call(Type, Call) ->
  case Type of
    read -> match_stream_db_reader:make_call(Call);
    write -> match_stream_db_writer:make_call(Call);
    write_once ->
      case {node(), node(global:whereis_name(match_stream_db_writer))} of
        {Here, Here} ->
          match_stream_db_writer:make_call(Call);
        {_, _} -> %%HACK: It will be written by the local node, no need to call it from here too
          ok
      end
  end.