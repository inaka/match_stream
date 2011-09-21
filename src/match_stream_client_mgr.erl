%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 InakaLabs S.R.L.
%%% @doc Supervisor for Client Processes
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_client_mgr).

-include("match_stream.hrl").

-behaviour(supervisor).

-export([start_link/1, start_client/0, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link(atom()) -> ignore | {error, term()} | {ok, pid()}.
start_link(Name) ->
	supervisor:start_link({local, Name}, ?MODULE, []).

%% @doc  Starts a new client process
-spec start_client() -> {ok, pid() | undefined} | {error, term()}.
start_client() ->
  supervisor:start_child(?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{simple_one_for_one, 100, 1}, [supervisor:child_spec()]}}.
init([]) ->
  {ok, {{simple_one_for_one, 100, 1},
        [{match_stream_client, {match_stream_client, start_link, []},
          temporary, brutal_kill, worker, [match_stream_client]}]}}.