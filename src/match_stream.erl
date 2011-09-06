%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2011 InakaLabs SRL
%% @doc Match Stream main interface
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
-module(match_stream).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').
-vsn('0.1').

-behaviour(application).

-include("match_stream.hrl").

-export([start/0, stop/0]).
-export([start/2, stop/1]).
-export([new_match/2, register_event/1]).

-type team() :: binary().
-type match_id() :: binary().
-type event_kind() :: start | stop | shot | offside | foul | corner | throwin | penalty | freekick | substitution | goal.
-type event() :: #match_stream_event{}.

%%-------------------------------------------------------------------
%% ADMIN API
%%-------------------------------------------------------------------
%% @doc Starts the application
-spec start() -> ok | {error, {already_started, match_stream}}.
start() ->
  _ = application:start(public_key),
  _ = application:start(ssl),
  application:start(match_stream).

%% @doc Stops the application
-spec stop() -> ok.
stop() -> application:stop(rpcio).

%%-------------------------------------------------------------------
%% SERVER API
%%-------------------------------------------------------------------
%% @doc Registers a match
-spec new_match(team(), team()) -> match_id().
new_match(Home, Visiting) ->
  match_stream_db:register_game(Home, Visiting).

%% @doc Something happened in a match
-spec register_event(event()) -> ok.
register_event(Event) ->
  match:apply(Event).



%%-------------------------------------------------------------------
%% BEHAVIOUR CALLBACKS
%%-------------------------------------------------------------------
%% @private
-spec start(any(), any()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  case match_stream_sup:start_link() of
    {ok, Pid} -> {ok, Pid};
    Error -> {error, Error}
  end.

%% @private
-spec stop(any()) -> ok.
stop(_State) -> ok.