%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Match Stream Database.
%%% It uses a Redis backend.
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_db).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("match_stream.hrl").

-define(REDIS_CONNECTIONS, 100).

-record(state, {redis :: [pid()]}).
-opaque state() :: #state{}.

-export([create/1, match_update/2, user_update/2, match_all/0, user_all/0, match/1, user/1,
         match_delete/1, user_delete/1, event_log/1, match_history/1]).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% External functions
%% =================================================================================================
%% @doc Creates a match or an user
%% @throws duplicated
-spec create(match_stream:match() | match_stream:user()) -> ok.
create(Object = #match_stream_match{match_id = MatchId}) ->
  make_call({create, <<"match-", MatchId/binary>>, Object});
create(Object = #match_stream_user{user_id = UserId}) ->
  make_call({create, <<"user-", UserId/binary>>, Object}).

%% @doc Updates an existing match
%% @throws not_found
-spec match_update(match_stream:match_id(), fun((match_stream:match()) -> match_stream:match())) -> ok.
match_update(MatchId, UpdateFun) ->
  make_call({update, <<"match-", MatchId/binary>>, UpdateFun}).

%% @doc Updates an existing user
%% @throws not_found
-spec user_update(match_stream:user_id(), fun((match_stream:user()) -> match_stream:user())) -> ok.
user_update(UserId, UpdateFun) ->
  make_call({update, <<"user-", UserId/binary>>, UpdateFun}).

%% @doc Returns the list of available matches
-spec match_all() -> [match_stream:match_id()].
match_all() ->
  make_call({all, "match-*"}).

%% @doc Returns the list of available users
-spec user_all() -> [match_stream:user_id()].
user_all() ->
  make_call({all, "user-*"}).

%% @doc Returns a match
-spec match(match_stream:match_id()) -> match_stream:match() | not_found.
match(MatchId) ->
  make_call({get, <<"match-", MatchId/binary>>}).

%% @doc Returns an user
-spec user(match_stream:user_id()) -> match_stream:user() | not_found.
user(UserId) ->
  make_call({get, <<"user-", UserId/binary>>}).

%% @doc Deletes a match.
%% Silently ignores the call if the match is not there
-spec match_delete(match_stream:match_id()) -> ok.
match_delete(MatchId) ->
  make_call({delete, <<"match-", MatchId/binary>>}),
  make_call({delete_events, MatchId}).

%% @doc Deletes an user.
%% Silently ignores the call if the user is not there
-spec user_delete(match_stream:user_id()) -> ok.
user_delete(UserId) ->
  make_call({delete, <<"user-", UserId/binary>>}).

%% @doc Logs an event
%% @throws not_found
-spec event_log(match_stream:event()) -> ok.
event_log(Event) ->
  make_call({log, Event}).

%% @doc Returns match events
-spec match_history(match_stream:match_id()) -> [match_stream:event()].
match_history(MatchId) ->
  make_call({match_history, MatchId}).

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
  Redis =
    lists:map(
      fun(_) ->
              {ok, Conn} =
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
              Conn
      end, lists:seq(1, ?REDIS_CONNECTIONS)),
  ?INFO("Database initialized~n", []),
  {ok, #state{redis = Redis}}.

%% @hidden
-spec handle_call(tuple(), reference(), state()) -> {reply, {ok, term()} | {throw, term()}, state()}.
handle_call(Request, From, State) ->
  [RedisConn|Redis] = lists:reverse(State#state.redis),
  proc_lib:spawn_link(
    fun() ->
            Res =
              try handle_call(Request, RedisConn) of
                ok -> ok;
                Result -> {ok, Result}
              catch
                throw:Error ->
                  {throw, Error}
              end,
            gen_server:reply(From, Res)
    end),
  {noreply, State#state{redis = [RedisConn|lists:reverse(Redis)]}}.

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

-spec handle_call(term(), pid()) -> term().
handle_call({create, Key, Object}, RedisConn) ->
  case erldis:get(RedisConn, Key) of
    nil ->
      case erldis:set(RedisConn, Key, Object) of
        ok -> ok;
        Error -> throw(Error)
      end;
    _ ->
      throw(duplicated)
  end;
handle_call({update, Key, UpdateFun}, RedisConn) ->
  case erldis:get(RedisConn, Key) of
    nil ->
      throw(not_found);
    MatchBin ->
      case erldis:set(RedisConn, Key, UpdateFun(erlang:binary_to_term(MatchBin))) of
        ok -> ok;
        Error -> throw(Error)
      end
  end;
handle_call({delete, Key}, RedisConn) ->
  _Removed = erldis:del(RedisConn, Key),
  ok;
handle_call({delete_events, MatchId}, RedisConn) ->
  Keys =
    case erldis:keys(RedisConn, "event-" ++ binary_to_list(MatchId) ++ "-*") of
      [] -> [];
      [Bin1] -> binary:split(Bin1, <<" ">>, [global, trim]);
      Ids -> Ids
    end,
  _RemovedEvents = erldis:delkeys(RedisConn, Keys),
  ok;
handle_call({log, Event}, RedisConn) ->
  MatchKey = <<"match-", (Event#match_stream_event.match_id)/binary>>,
  case erldis:get(RedisConn, MatchKey) of
    nil ->
      throw(not_found);
    _Match ->
      Key = <<"event-", (Event#match_stream_event.match_id)/binary, $-,
              (list_to_binary(integer_to_list(Event#match_stream_event.timestamp)))/binary>>,
      case erldis:set(RedisConn, Key, Event) of
        ok -> ok;
        Error -> throw(Error)
      end
  end;
handle_call({all, Prefix}, RedisConn) ->
  PrefixLength = length(Prefix) - 1,
  Res =
    case erldis:keys(RedisConn, Prefix) of
      [] -> [];
      [Bin1] -> binary:split(Bin1, <<" ">>, [global, trim]);
      Ids -> Ids
    end,
  lists:map(fun(<<_:PrefixLength/binary, MatchId/binary>>) -> MatchId end, Res);
handle_call({match_history, MatchId}, RedisConn) ->
  Keys =
    case erldis:keys(RedisConn, "event-" ++ binary_to_list(MatchId) ++ "-*") of
      [] -> [];
      [Bin1] -> binary:split(Bin1, <<" ">>, [global, trim]);
      Ids -> Ids
    end,
  lists:keysort(
    #match_stream_event.timestamp,
    lists:foldl(
      fun(Key, Acc) ->
              case erldis:get(RedisConn, Key) of
                nil -> Acc;
                Bin -> [erlang:binary_to_term(Bin)|Acc]
              end
      end, [], Keys));
handle_call({get, Key}, RedisConn) ->
  case erldis:get(RedisConn, Key) of
    nil -> not_found;
    MatchBin -> erlang:binary_to_term(MatchBin)
  end.