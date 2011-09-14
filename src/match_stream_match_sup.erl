%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 InakaLabs S.R.L.
%%% @doc Supervisor for Match Processes
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_match_sup).

-include("match_stream.hrl").

-behaviour(supervisor).

-export([start_link/0, start_match/1, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc  Starts a new client process
-spec start_match(match_stream:match_id()) -> {ok, pid()} | {error, term()}.
start_match(Match) ->
  supervisor:start_child(?MODULE, [Match]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{simple_one_for_one, 100, 1}, [supervisor:child_spec()]}}.
init([]) ->
  ?INFO("Match supervisor initialized~n", []),
  {ok, {{simple_one_for_one, 100, 1},
        [{match_stream_match, {match_stream_match, start_link, []},
          transient, brutal_kill, worker, [match_stream_match]}]}}.
