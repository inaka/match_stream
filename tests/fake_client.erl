-module(fake_client).

-export([watch/4, watch/5]).

%% @doc Spawn links N watchers
-spec watch(pos_integer(), inet:ip_address() | inet:hostname(), pos_integer(), match_stream:user_id(), match_stream:match_id()) -> ok.
watch(N, Server, Port, UserIdPrefix, MatchId) ->
  lists:foreach(fun(I) ->
                        UserId = <<UserIdPrefix/binary, $.,
                                   (list_to_binary(integer_to_list(I)))/binary>>,
                        proc_lib:spawn_link(
                          fun() ->
                                  {T, ok} =
                                    timer:tc(fun() -> watch(Server, Port, UserId, MatchId) end),
                                  io:format("Tester ~s done - ~p ms~n", [UserId, T])
                          end)
                end, lists:seq(1, N)).

%% @doc Connects to the server and watches a game till it ends
-spec watch(inet:ip_address() | inet:hostname(), pos_integer(), match_stream:user_id(), match_stream:match_id()) -> ok.
watch(Server, Port, UserId, MatchId) ->
  case gen_tcp:connect(Server, Port, [binary, {packet, line}]) of
    {ok, Socket} ->
      gen_tcp:send(Socket, <<"VERSION:1:CONNECT:", UserId/binary, ":MATCH:", MatchId/binary, "\r\n">>),
      loop(Socket);
    Error ->
      throw(Error)
  end.

-spec loop(port()) -> ok.
loop(Socket) ->
  loop(Socket, [{<<"connect">>, complete}]).

-spec loop(port(), [binary()]) -> ok.
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
      io:format("~p closed~n", [self()]),
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
      io:format("Event on (~p): ~s~n", [self(), Event]),
      loop(Socket, [{Event, complete}|Events]);
    {tcp, Socket, Msg} -> %% A header line, we add the event to the list
      throw({incomplete_event, Event, Msg});
    {tcp_closed, Socket} ->
      throw({disconnected_on, Event})
  end.