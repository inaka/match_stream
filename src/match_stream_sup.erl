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

-behaviour(supervisor).

-export([start_link/0, init/1]).

%% @doc  Starts a new supervisor
-spec start_link() -> {ok, pid()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @hidden
-spec init([]) -> {ok, {{one_for_one, 5, 10}, [supervisor:child_spec()]}}.
init([]) ->
  Port = case application:get_env(port) of
           undefined -> 9999;
           P -> P
         end,
  Listener = {match_stream_listener, {match_stream_listener, start_link, [Port]},
              permanent, 1000, worker, [match_stream_listener]},
  ClientSup = {match_stream_client_sup, {match_stream_client_sup, start_link, []},
               permanent, 1000, supervisor, [match_stream_client_sup]},
  UserSup = {match_stream_user_sup, {match_stream_user_sup, start_link, []},
             permanent, 1000, supervisor, [match_stream_user_sup]},
  {ok, {{one_for_one, 5, 10}, [Listener, UserSup, ClientSup]}}.