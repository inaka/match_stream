%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Match Stream Database Writer.
%%% It uses a Redis backend.
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_db_writer).
-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-behaviour(gen_server).

-include("match_stream.hrl").

-define(REDIS_CONNECTIONS, 100).

-record(state, {redis :: [pid()]}).
-opaque state() :: #state{}.

-export([start_link/0, make_call/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% =================================================================================================
%% Internal (i.e. used only by other modules) functions
%% =================================================================================================
%% @hidden
-spec start_link() -> {ok, pid()}.
start_link() ->
  case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
    {ok, Pid} -> {ok, Pid};
    {error, {already_started, Pid}} ->
      link(Pid),
      {ok, Pid}
  end.

%% @hidden
-spec make_call(tuple()) -> ok.
make_call(Call) ->
  gen_server:cast({global, ?MODULE}, Call).

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
  ?INFO("Database writer initialized~n", []),
  {ok, #state{redis = Redis}}.

%% @hidden
-spec handle_call(X, reference(), #state{}) -> {stop, {unknown_request, X}, {unknown_request, X}, #state{}}.
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

%% @hidden
-spec handle_cast(_, state()) -> {noreply, state()}.
handle_cast(Request, State) ->
  [RedisConn|Redis] = lists:reverse(State#state.redis),
  try handle_call(Request, RedisConn) of
    ok ->
      {noreply, State#state{redis = [RedisConn|lists:reverse(Redis)]}}
  catch
    throw:Error ->
      ?ERROR("Error trying to process write request:~n\tReq:\t~p~nError:\t~p~n", [Request, Error]),
      {stop, Error, State}
  end.

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
  end.