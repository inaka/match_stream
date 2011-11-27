%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc Match Stream main supervisor
%% @end
%%-------------------------------------------------------------------
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
-module(match_stream_sup).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-include("match_stream.hrl").

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Listener = {match_stream_client_listener_sup, {match_stream_client_listener_sup, start_link, []},
              permanent, 1000, supervisor, [match_stream_client_listener_sup]},
  ClientSup = {match_stream_client_sup, {match_stream_client_sup, start_link, []},
               permanent, 1000, supervisor, [match_stream_client_sup]},
  UserSup = {match_stream_user_sup, {match_stream_user_sup, start_link, []},
             permanent, 1000, supervisor, [match_stream_user_sup]},
  MatchSup = {match_stream_match_sup, {match_stream_match_sup, start_link, []},
             permanent, 1000, supervisor, [match_stream_match_sup]},
  MatchDb = {match_stream_db, {match_stream_db, start_link, []},
              permanent, 1000, supervisor, [match_stream_db]},
  ?INFO("Main supervisor initialized~n", []),
  {ok, {{one_for_one, 5, 10}, [MatchDb, Listener, MatchSup, UserSup, ClientSup]}}.
