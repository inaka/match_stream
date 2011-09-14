%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 InakaLabs S.R.L.
%%% @doc Supervisor for User Processes
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_user_sup).

-behaviour(supervisor).

-export([start_link/0, start_user/1, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc  Starts a new client process
-spec start_user(match_stream:user_id()) -> {ok, pid()} | {error, term()}.
start_user(User) ->
  supervisor:start_child(?MODULE, [User]).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{simple_one_for_one, 100, 1}, [supervisor:child_spec()]}}.
init([]) ->
    {ok, {{simple_one_for_one, 100, 1},
            [{match_stream_user, {match_stream_user, start_link, []},
              transient, brutal_kill, worker, [match_stream_user]}]}}.