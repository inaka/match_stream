%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <greenmellon@gmail.com>
%%% @copyright (C) 2011 Kotoko Group
%%% @doc Match Stream Database.
%%% It uses a Redis backend.
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_db).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("match_stream.hrl").

-record(state, {redis :: pid()}).
-opaque state() :: #state{}.

-export([create/1, update/2, get/1, delete/1, log/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @doc Creates a match
%% @throws duplicated
-spec create(match_stream:match()) -> ok.
create(Match) ->
  make_call({create, Match}).

%% @doc Updates an existing match
%% @throws not_found
-spec update(match_stream:match_id(), fun((match_stream:match()) -> match_stream:match())) -> ok.
update(MatchId, UpdateFun) ->
  make_call({update, MatchId, UpdateFun}).

%% @doc Returns a match
-spec get(match_stream:match_id()) -> match_stream:match() | not_found.
get(MatchId) ->
  make_call({get, MatchId}).

%% @doc Deletes a match.
%% Silently ignores the call if the match is not there
-spec delete(match_stream:match_id()) -> ok.
delete(MatchId) ->
  make_call({delete, MatchId}).

%% @doc Logs an event
%% @throws not_found
-spec log(match_stream:event()) -> ok.
log(Event) ->
  make_call({log, Event}).

%% =================================================================================================
%% Internal (i.e. used only by other modules) functions
%% =================================================================================================
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% =================================================================================================
%% Server functions
%% =================================================================================================
%% @hidden
-spec init([]) -> {ok, state()}.
init([]) ->
  Host = case application:get_env(erldis, host) of
           {ok, H} -> H;
           undefined -> "localhost"
         end,
  Port = case application:get_env(erldis, port) of
           {ok, P} -> P;
           undefined -> 6379
         end,
  Timeout = case application:get_env(erldis, timeout) of
              {ok, T} -> T;
              undefined -> 500
            end,
  {ok, Redis} =
    case {application:get_env(redis_pwd), application:get_env(redis_db)} of
      {undefined, undefined} ->
        erldis_client:start_link();
      {undefined, {ok, Db}} ->
        erldis_client:start_link(Db);
      {{ok, Pwd}, undefined} ->
        erldis_client:start_link(Host, Port, Pwd);
      {{ok, Pwd}, {ok, Db}} ->
        erldis_client:start_link(Host, Port, Pwd, [{timeout, Timeout}], Db)
    end,
  {ok, #state{redis = Redis}}.

%% @hidden
-spec handle_call(tuple(), reference(), state()) -> {reply, {ok, term()} | {throw, term()}, state()}.
handle_call({create, Match}, _From, State) ->
  case erldis:get(State#state.redis, Match#match_stream_match.match_id) of
    nil ->
      try erldis:set(State#state.redis, Match#match_stream_match.match_id, Match) of
        ok -> {reply, ok, State};
        Error -> {reply, {throw, Error}, State}
      catch
        throw:Error -> {reply, {throw, Error}, State}
      end;
    _ ->
      {reply, {throw, duplicated}, State}
  end;
handle_call({update, MatchId, UpdateFun}, _From, State) ->
  case erldis:get(State#state.redis, MatchId) of
    nil ->
      {reply, {throw, not_found}, State};
    MatchBin ->
      try erldis:set(State#state.redis, MatchId,
                     UpdateFun(erlang:binary_to_term(MatchBin))) of
        ok -> {reply, ok, State};
        Error -> {reply, {throw, Error}, State}
      catch
        throw:Error -> {reply, {throw, Error}, State}
      end
  end;
handle_call({delete, MatchId}, _From, State) ->
  try
    _Removed = erldis:del(State#state.redis, MatchId),
    {reply, ok, State}
  catch
    throw:Error -> {reply, {throw, Error}, State}
  end;
handle_call({log, Event}, _From, State) ->
  case erldis:get(State#state.redis, Event#match_stream_event.match_id) of
    nil ->
      {reply, {throw, not_found}, State};
    _Match ->
      Key = list_to_binary(integer_to_list(Event#match_stream_event.timestamp)),
      try erldis:set(State#state.redis, Key, Event) of
        ok -> {reply, ok, State};
        Error -> {reply, {throw, Error}, State}
      catch
        throw:Error -> {reply, {throw, Error}, State}
      end
  end;
handle_call({get, MatchId}, _From, State) ->
  case erldis:get(State#state.redis, MatchId) of
    nil ->
      {reply, {ok, not_found}, State};
    MatchBin ->
      {reply, {ok, erlang:binary_to_term(MatchBin)}, State}
  end.

%% @hidden
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(_, State) -> {noreply, State}.

%% @hidden
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_, State) -> {noreply, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_, _) -> ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% =================================================================================================
%% Private functions
%% =================================================================================================
-spec make_call(tuple()) -> ok | not_found | match_stream:match().
make_call(Call) ->
  case gen_server:call(?MODULE, Call) of
    ok -> ok;
    {ok, Result} -> Result;
    {throw, Exception} -> throw(Exception)
  end.