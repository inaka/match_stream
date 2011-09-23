%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 InakaLabs S.R.L.
%%% @doc Supervisor for Match Processes
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_match_sup).

-include("match_stream.hrl").

-behaviour(supervisor).

-export([start_link/0, match_pids/1, stop_match/1, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link() -> {ok, pid()}.
start_link() ->
  %% First of all we create/join the group of all supervisors...
  ok = pg2:create(?MODULE),
  Pid = case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
          {ok, P} -> P;
          {error, {already_started, P}} -> P
        end,
  %% Then we join it...
  ok = pg2:join(?MODULE, Pid),
  {ok, Pid}.

%% @doc  Returns the list of processes related to that match.
%%       Starts a new match process in every node if not already there 
-spec match_pids(match_stream:match_id()) -> [pid()].
match_pids(Match) ->
  case pg2:get_members(?MODULE) of
    {error, Reason} -> throw(Reason);
    Sups ->
      lists:map(
        fun(Sup) ->
                case supervisor:start_child(Sup, [Match]) of
                  {ok, P} -> P;
                  {error, {already_started, P}} -> P
                end
        end, Sups)
  end.

%% @doc  Stops a match process
-spec stop_match(match_stream:match_id()) -> ok.
stop_match(MatchId) ->
  case pg2:get_members(?MODULE) of
    {error, Reason} -> throw(Reason);
    Sups ->
      lists:foreach(
        fun(Sup) ->
                rpc:call(node(Sup), match_stream_match, stop, [MatchId])
        end, Sups)
  end.

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
