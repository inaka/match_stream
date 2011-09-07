%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
%%% @doc Match Stream User
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_user).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-record(state, {}).
-opaque state() :: #state{}.

-export([watch/3, notify/2]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @doc Something happened in a match
-spec notify(match_stream:user_id(), match_stream:event()) -> ok.
notify(User, Event) ->
  gen_server:cast(process(User), Event).

%% @doc User wants to watch Match through Client
-spec watch(match_stream:user_id(), match_stream:match_id(), pid()) -> ok | {error, term()}.
watch(User, Match, Client) ->
  UserPid =
    case match_stream_user_sup:start_user(User) of
      {ok, Pid} -> Pid;
      {error, {already_started, Pid}} -> Pid
    end,
  gen_server:call(UserPid, {watch, Match, Client}).

%% @hidden
-spec start_link(match_stream:user_id()) -> {ok, pid()}.
start_link(User) ->
  gen_server:start_link({local, process(User)}, ?MODULE, User, []).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init([]) -> {ok, state()}.
init([]) -> {ok, #state{}}.

%% @hidden
-spec handle_call({req, string(), ibrowse:headerList(), ibrowse:method(), ibrowse:body(), ibrowse:optionList(), pos_integer()|infinity}, reference(), state()) -> {reply, {ok, ibrowse:response()} | {throw|error, term()}, state()}.
handle_call({req, Url, Headers, Method, Body, Options, Timeout}, _From, State) ->
  Res =
    try ibrowse:send_req(Url, Headers, Method, Body, Options, Timeout) of
      R -> {ok, R}
    catch
      X:E -> {X, E}
    end,
  {reply, Res, State}.

%% @hidden
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state(), hibernate}.
handle_info(_, State) -> {noreply, State, hibernate}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =================================================================================================
%% Private functions
%% =================================================================================================
process(User) -> binary_to_atom(<<(atom_to_binary(?MODULE, utf8)), $-, User/binary>>, utf8).