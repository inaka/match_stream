%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 InakaLabs S.R.L.
%%% @doc Supervisor for Client Listener Processes
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_client_listener_sup).

-include("match_stream.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  ?INFO("Listener supervisor initialized~n", []),
  {MinPort, MaxPort} =
    case application:get_env(listener_port_range) of
      {ok, Ports} -> Ports;
      undefined -> {9999,9999}
    end,
  Listeners =
    [{list_to_atom("match-stream-client-listener-" ++ integer_to_list(I)),
      {match_stream_client_listener, start_link, [I]},
      permanent, brutal_kill, worker, [match_stream_client_listener]}
     || I <- lists:seq(MinPort, MaxPort)],
  {ok, {{one_for_one, 5, 10}, Listeners}}.