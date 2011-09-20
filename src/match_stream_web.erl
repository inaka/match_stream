%%%-------------------------------------------------------------------
%%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%%% @copyright (C) 2011 Inaka Labs SRL
%%% @doc Match Stream API web server
%%% @end
%%%-------------------------------------------------------------------
-module(match_stream_web).

-author('Fernando Benavides <fernando.benavides@inakanetworks.com>').

-include("match_stream.hrl").

-export([start_link/0, loop/1]).

-define(VERSION, "1").
-define(BASE_HEADERS, [{"Access-Control-Allow-Origin",  "*"},
                       {"Access-Control-Allow-Methods", "GET,  POST, OPTIONS"},
                       {"Access-Control-Allow-Headers", "Content-Type"},
                       {"Access-Control-Max-Age",       "86400"},
                       {"Match-Stream-API-Version",     "2"}]).
-define(TEXT_HEADERS, [{"Content-Type", "text/plain; charset=utf-8"} | ?BASE_HEADERS]).

%% @private
-spec start_link() -> {ok, pid()}.
start_link() ->
  Port = case application:get_env(web_port) of
           undefined -> 8888;
           {ok, P} -> P
         end,
  ?INFO("Web player handler starting on port ~p~n", [Port]),
  mochiweb_http:start([{name, ?MODULE}, {loop, {?MODULE, loop}}, {backlog, 128000}, {port, Port}]).

%% @private
-spec loop(atom() | tuple()) -> ok.
loop(Req) ->
  Path = Req:get(path),
  
  Prev = match_stream:timestamp(),
  try
    case {Req:get(method), string:tokens(Path, "/")} of
      {'GET',  [?VERSION, "matches" | _RestOfPath]} ->
        Matches = match_stream:matches(),
        Req:ok({_ContentType = "application/json", _Headers = ?BASE_HEADERS,
                mochijson2:encode(Matches)});

      {'GET',  [?VERSION, "match", MatchId, "history" | _RestOfPath]} ->
        Events = match_stream:history(list_to_binary(MatchId)),
        Req:ok({_ContentType = "application/json", _Headers = ?BASE_HEADERS,
                mochijson2:encode(lists:map(fun to_json/1, Events))});

      {'GET',  [?VERSION, "match", MatchId | _RestOfPath]} ->
        case match_stream:match(list_to_binary(MatchId)) of
          not_found -> throw({not_found, MatchId});
          Match -> 
            Req:ok({_ContentType = "application/json", _Headers = ?BASE_HEADERS,
                    mochijson2:encode(to_json(Match))})
        end;

      X ->
        ?WARN("Couldn't match anything: ~p ~n", [X]),
        Req:respond({404, ?TEXT_HEADERS, "404 - Sorry, not found\r\n"})
    end
  catch
    _:{not_found, Name} ->
      ?WARN("~p not found~nPath: ~p~n~p~n", [Name, Path, erlang:get_stacktrace()]),
      Req:respond({404, ?TEXT_HEADERS, "404 Not found: " ++ Name ++ "\r\n"});
    _:{missing_parameter, Name} ->
      ?WARN("Misssing parameter: ~p~nPath: ~p~n~p~n", [Name, Path, erlang:get_stacktrace()]),
      Req:respond({400, ?TEXT_HEADERS, "400 Bad Request -  Missing parameter: " ++ Name ++ "\r\n"});
    _:{error, {Error, Desc}} ->
      ?WARN("Error: ~p~nPath: ~p~n~p~n", [{Error, Desc}, Path, erlang:get_stacktrace()]),
      Msg = io_lib:format("500 Runtime error - ~p ~n", [Error]),
      Req:respond({500, ?TEXT_HEADERS, Msg});
    _:{error, Error} ->
      ?WARN("Error: ~p~nPath: ~p~n~p~n", [Error, Path, erlang:get_stacktrace()]),
      Msg = io_lib:format("500 Runtime error - ~p ~n", [Error]),
      Req:respond({500, ?TEXT_HEADERS, Msg});
    _:{Error, Desc} ->
      ?WARN("Error: ~p~nPath: ~p~n~p~n", [{Error, Desc}, Path, erlang:get_stacktrace()]),
      Msg = io_lib:format("500 Runtime error - ~p ~n", [Error]),
      Req:respond({500, ?TEXT_HEADERS, Msg});
    _:Error ->
      ?WARN("Error: ~p~nPath: ~p~n~p~n", [Error, Path, erlang:get_stacktrace()]),
      Msg = io_lib:format("500 Runtime error - ~p ~n", [Error]),
      Req:respond({500, ?TEXT_HEADERS, Msg})
  end,
  ?DEBUG("~p: ~p - ~p ms~n", [Req:get(method), Path, match_stream:timestamp() - Prev]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PRIVATE FUNCTIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
to_json(#match_stream_match{home = H, home_players = HPs, home_score = HS,
                            period = P, start_time = {Y, M, D},
                            visit = V, visit_players = VPs, visit_score = VS}) ->
  {struct, [{<<"home">>, H},
            {<<"home-players">>, HPs},
            {<<"home-score">>, HS},
            {<<"visit">>, V},
            {<<"visit-players">>, VPs},
            {<<"visit-score">>, VS},
            {<<"period">>, P},
            {<<"start_time">>, iolist_to_binary([lpad(Y), $-, lpad(M), $-, lpad(D)])}]};
to_json(#match_stream_event{timestamp = TS, kind = K, data = D}) ->
  {struct, [{<<"timestamp">>, TS},
            {<<"kind">>, atom_to_binary(K, utf8)} |
              case D of
                [] -> [];
                D -> [{<<"data">>, lists:map(fun to_json/1, D)}]
              end
            ]};
to_json({K, Players}) when K == home_players; K == visit_players ->
  {atom_to_binary(K, utf8),
   lists:map(fun({B, M}) ->
                     {list_to_binary(integer_to_list(B)), M}
             end, Players)};
to_json({K, {B,M}}) ->
  {atom_to_binary(K, utf8), {struct, [{list_to_binary(integer_to_list(B)), M}]}};
to_json({K, V}) when is_list(V) ->
  {atom_to_binary(K, utf8), list_to_binary(V)};
to_json({K, V}) when is_atom(V) ->
  {atom_to_binary(K, utf8), atom_to_binary(V, utf8)};
to_json({K, V}) ->
  {atom_to_binary(K, utf8), V}.

lpad(X) when X < 10 -> [$0 | integer_to_list(X)];
lpad(X) -> integer_to_list(X).