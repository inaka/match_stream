%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2010 InakaLabs S.R.L.
%%% @doc Supervisor for Client Processes
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_client_sup).

-include("match_stream.hrl").
-define(MANAGERS, 400). %%NOTE: To reduce message_queue_lens on massive client initialization

-behaviour(supervisor).

-export([start_link/0, start_client/0, init/1, count_clients/0]).

%% ====================================================================
%% External functions
%% ====================================================================
%% @doc  Starts the supervisor process
-spec start_link() -> ignore | {error, term()} | {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc  Starts a new client process
-spec start_client() -> {ok, pid() | undefined} | {error, term()}.
start_client() ->
  _ = random:seed(erlang:now()),
  Manager =
    list_to_atom("match-stream-client-manager-" ++ integer_to_list(random:uniform(?MANAGERS))),
  supervisor:start_child(Manager, []).

%% @doc  Returns the count of reigstered clients under the supervision of this process
-spec count_clients() -> non_neg_integer().
count_clients() ->
  lists:sum(
    lists:map(
      fun(I) ->
              proplists:get_value(
                active,
                supervisor:count_children(
                  list_to_atom("match-stream-client-manager-" ++ integer_to_list(I))),
                0)
      end, lists:seq(1, ?MANAGERS))).

%% ====================================================================
%% Server functions
%% ====================================================================
%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  ?INFO("User supervisor initialized~n", []),
  Managers =
    [{list_to_atom("match-stream-client-manager-" ++ integer_to_list(I)),
      {match_stream_client_mgr, start_link,
       [list_to_atom("match-stream-client-manager-" ++ integer_to_list(I))]},
      permanent, brutal_kill, supervisor, [match_stream_client_mgr]}
     || I <- lists:seq(1, ?MANAGERS)],
  {ok, {{one_for_one, 5, 10}, Managers}}.