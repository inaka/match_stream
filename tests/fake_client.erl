-module(fake_client).

-export([watch/3, watch/5]).

%% @doc Spawn links N watchers
-spec watch(pos_integer(), pos_integer(), inet:ip_address() | inet:hostname(), pos_integer(), match_stream:user_id()) -> {non_neg_integer(), non_neg_integer()}.
watch(N, C, Server, Port, UserIdPrefix) ->
  Self = self(),
  lists:foreach(
    fun(I) ->
            io:format("Starting clients ~p to ~p...~n", [I+1, I+C]),
            lists:foreach(
              fun(IC) ->
                      UserId = <<UserIdPrefix/binary, $.,
                                 (list_to_binary(integer_to_list(IC)))/binary>>,
                      proc_lib:spawn_link(
                        fun() ->
                                try
                                  {T, {ok, RT}} =
                                      timer:tc(?MODULE, watch, [Server, Port, UserId]),
                                  io:format("Tester ~s done - ~p s~n", [UserId, erlang:round(T/1000000)]),
                                  Self ! {ok, RT}
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
  {ROk, RSRT, RErr} =
    lists:foldl(fun(_, {Ok, SRT, Err}) ->
                        io:format("Waiting for result #~p~n", [P]), 
                        receive
                          {ok, RT} -> {Ok+1, SRT + RT, Err};
                          error -> {Ok, SRT, Err+1};
                          Other -> io:format("WTF?? ~p~n", [Other]), {Ok, SRT, Err+1}
                        end
                end, {0, 0, 0}, lists:seq(1, N)),
  {ROk, erlang:round(RSRT/ROk), RErr}.

%% @doc Connects to the server and watches a game till it ends
-spec watch(inet:ip_address() | inet:hostname(), pos_integer(), match_stream:user_id()) -> ok.
watch(Server, Port, UserId) ->
  case gen_tcp:connect(Server, Port, [binary, {packet, line}]) of
    {ok, Socket} ->
      run(UserId, Socket);
    Error ->
      throw(Error)
  end.

-spec run(binary(), port()) -> {ok, pos_integer()}.
run(UserId, Socket) ->
  wait_for_welcome(UserId, Socket, [], timestamp()).

-spec wait_for_welcome(binary(), port(), [binary()], pos_integer()) -> {ok, pos_integer()}.
wait_for_welcome(UserId, Socket, [], Ts) ->
  receive
    {tcp, Socket, <<"Welcome to Match Stream.", _/binary>>} ->
      RT = timestamp() - Ts,
      io:format("~p. ~p. Welcome message in ~pms.~n", [self(), Socket, RT]),
      wait_for_welcome(UserId, Socket, [], RT);
    {tcp, Socket, <<"To watch the current game use: V:2:<your-name>", _/binary>>} ->
      gen_tcp:send(Socket, <<"V:2:", UserId/binary, "\r\n">>),
      {ok, FRT} = wait_for_first_event(Socket, [{<<"connect">>, complete}], timestamp()),
      {ok, erlang:max(Ts, FRT)}
  end.

-spec wait_for_first_event(port(), [binary()], pos_integer()) -> {ok, pos_integer()}.
wait_for_first_event(Socket, Events, Ts) ->
  receive
    {tcp, Socket, <<$\t, Msg/binary>>} -> %% A detail line, error
      throw({detail_out_of_event, Msg});
    {tcp, Socket, <<"\r\n">>} -> %% A closing line, error
      throw(empty_event);
    {tcp, Socket, Msg} -> %% A header line, we add the event to the list
      RT = timestamp() - Ts,
      case binary:split(Msg, [<<": ">>,<<":\n">>], [global, trim]) of
        [_Date, Ev] ->
          io:format("~p. ~p. First message in ~pms.~n", [self(), Socket, RT]),
          {loop(Socket, [Ev|Events]), RT};
        _ ->
          throw({invalid_header, Msg})
      end;
    {tcp_closed, Socket} ->
      io:format("~p closed~n", [self()]),
      throw({incomplete_match, hd(Events)})
  end.

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
      %io:format("Event on (~p): ~s~n", [self(), Event]),
      loop(Socket, [{Event, complete}|Events]);
    {tcp, Socket, Msg} -> %% A header line, we add the event to the list
      throw({incomplete_event, Event, Msg});
    {tcp_closed, Socket} ->
      throw({disconnected_on, Event})
  end.

timestamp() ->
  {_, _, MicroSecs} = erlang:now(),
  Millis = erlang:trunc(MicroSecs/1000),
  calendar:datetime_to_gregorian_seconds(
    calendar:universal_time()) * 1000 + Millis.