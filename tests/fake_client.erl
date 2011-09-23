-module(fake_client).

-export([watch/4, watch/5]).

%% @doc Spawn links N watchers
-spec watch(pos_integer(), pos_integer(), inet:ip_address() | inet:hostname(), match_stream:user_id(), match_stream:match_id()) -> {non_neg_integer(), non_neg_integer()}.
watch(N, C, Server, UserIdPrefix, MatchId) ->
  Self = self(),
  Ports = case application:get_env(match_stream, listener_port_range) of
            {ok, {Min, Max}} -> lists:seq(Min, Max);
            undefined -> [9999]
          end,
  lists:foreach(
    fun(I) ->
            io:format("Starting clients ~p to ~p...~n", [I+1, I+C]),
            lists:foreach(
              fun(IC) ->
                      UserId = <<UserIdPrefix/binary, $.,
                                 (list_to_binary(integer_to_list(IC)))/binary>>,
                      Port = lists:nth(1 + IC rem length(Ports), Ports),
                      proc_lib:spawn_link(
                        fun() ->
                                try
                                  {T, ok} =
                                      timer:tc(?MODULE, watch, [Server, Port, UserId, MatchId]),
                                  io:format("Tester ~s done - ~p s~n", [UserId, erlang:round(T/1000000)]),
                                  Self ! ok
                                catch
                                  _:Error ->
                                    io:format("Tester ~s failed: ~p.~n\t~p~n", [UserId, Error, erlang:get_stacktrace()]),
                                    Self ! error
                                end
                        end)
              end, lists:seq(I+1, I+C)),
            io:format("Hold on a second...~n", []),
            timer:sleep(250)
    end, lists:seq(0, N-1, C)),
  lists:foldl(fun(_, {Ok, Err}) ->
                      receive
                        ok -> {Ok+1, Err};
                        error -> {Ok, Err+1}
                      end
              end, {0, 0}, lists:seq(1, N)).

%% @doc Connects to the server and watches a game till it ends
-spec watch(inet:ip_address() | inet:hostname(), pos_integer(), match_stream:user_id(), match_stream:match_id()) -> ok.
watch(Server, Port, UserId, MatchId) ->
  case gen_tcp:connect(Server, Port, [binary, {packet, line}]) of
    {ok, Socket} ->
      case gen_tcp:send(Socket, <<"VERSION:1:CONNECT:", UserId/binary, ":MATCH:", MatchId/binary, "\r\n">>) of
        ok ->
          loop(Socket);
        {error, Reason} ->
          throw(Reason)
      end;
    Error ->
      throw(Error)
  end.

-spec loop(port()) -> ok.
loop(Socket) ->
  loop(Socket, [{<<"connect">>, complete}]).

-spec loop(port(), [binary() | {binary(), atom()}]) -> ok.
loop(Socket, [{Event, complete}|Events]) ->
  receive
    {tcp, Socket, <<$\t, Msg/binary>>} -> %% A detail line, we ignore it 
      throw({detail_out_of_event, Msg});
    {tcp, Socket, <<"\r\n">>} -> %% A closing line, we close the event
      throw(empty_event);
    {tcp, Socket, Msg} -> %% A header line, we add the event to the list
      case binary:split(Msg, [<<": ">>,<<":\n">>], [global, trim]) of
        [_Date, Ev] ->
          loop(Socket, [Ev|Events]);
        _ ->
          throw({invalid_header, Msg})
      end;
    {tcp_closed, Socket} ->
      io:format("~p closed. Last event: ~s~n", [self(), Event]),
      case Event of
        <<"stop">> -> ok;
        <<"status">> -> ok;
        Event -> throw({incomplete_match, Event})
      end
  end;
loop(Socket, [Event|Events]) ->
  receive
    {tcp, Socket, <<$\t, _/binary>>} -> %% A detail line, we ignore it 
      loop(Socket, [Event|Events]);
    {tcp, Socket, <<"\r\n">>} -> %% A closing line, we close the event
      %io:format("Event on (~p): ~s~n", [self(), Event]),
      loop(Socket, [{Event, complete}|Events]);
    {tcp, Socket, Msg} -> %% A header line, we add the event to the list
      throw({incomplete_event, Event, Msg});
    {tcp_closed, Socket} ->
      throw({disconnected_on, Event})
  end.