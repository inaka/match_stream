-module(fake_watcher).

-include("match_stream.hrl").

-export([from_file/1]).

-spec from_file(string()) -> ok.
from_file(Filename) ->
  {ok, Params} = file:consult(Filename),
  HomeParams = proplists:get_value(home, Params, []),
  Home = #match_stream_team{team_id = proplists:get_value(id, HomeParams, <<"home">>),
                            name = proplists:get_value(name, HomeParams, <<"home team">>),
                            division = proplists:get_value(division, HomeParams, <<"home division">>)},
  VisitParams = proplists:get_value(visit, Params, []),
  Visit = #match_stream_team{team_id = proplists:get_value(id, VisitParams, <<"visit">>),
                             name = proplists:get_value(name, VisitParams, <<"visit team">>),
                             division = proplists:get_value(division, VisitParams, <<"visit division">>)},
  StartDate = proplists:get_value(start_date, Params, element(1, calendar:local_time())),
  Stadium = proplists:get_value(stadium, Params, <<"Unknown Stadium">>),
  Steps = proplists:get_value(steps, Params, []),
  ok = match_stream:set_team(Home),
  ok = match_stream:set_team(Visit),
  ok = match_stream:cancel_match(Home#match_stream_team.team_id,
                                 Visit#match_stream_team.team_id, StartDate),
  timer:sleep(1000),
  {ok, MatchId} = match_stream:new_match(Home#match_stream_team.team_id,
                                         Visit#match_stream_team.team_id, StartDate, Stadium),
  lists:foreach(
    fun({sleep, Ms}) ->
            io:format("Sleeping ~p ms....~n", [Ms]),
            timer:sleep(Ms);
       ({Kind, PreData}) ->
            io:format("~p: ~p~n", [Kind, PreData]),
            match_stream:register_event(MatchId, Kind, parse(MatchId, Kind, PreData))
    end, Steps).

parse(_MatchId, start, Data) ->
  [{home_players,
    proplists:get_value(
      home_players, Data,
      [{I, "Player " ++ integer_to_list(I)} || I <- lists:seq(1, 11)])},
   {visit_players,
    proplists:get_value(
      visit_players, Data,
      [{I, "Player " ++ integer_to_list(I)} || I <- lists:seq(1, 11)])} |
     lists:keydelete(home_players, 1,
                     lists:keydelete(visit_players, 1, Data))
   ];
parse(MatchId, PlayerEvent, Data) when PlayerEvent == shot;
                                       PlayerEvent == save;
                                       PlayerEvent == goal;
                                       PlayerEvent == corner;
                                       PlayerEvent == offside;
                                       PlayerEvent == foul;
                                       PlayerEvent == penalty;
                                       PlayerEvent == freekick;
                                       PlayerEvent == card;
                                       PlayerEvent == throwin ->
  case proplists:get_value(player, Data) of
    {_, _} -> Data;
    undefined -> Data;
    Number ->
      #match_stream_match{home_players = HomePlayers,
                          home = Home, visit = Visit,
                          visit_players = VisitPlayers} =
                           match_stream_db:match(MatchId),
      case {proplists:get_value(player_team, Data,
                                proplists:get_value(team, Data)),
            Home#match_stream_team.team_id, Visit#match_stream_team.team_id} of
        {HomeId, HomeId, _} ->
          [{player, {Number, proplists:get_value(Number, HomePlayers, <<"N/N">>)}} |
             lists:keydelete(player, 1, Data)];
        {VisitId, _, VisitId} ->
          [{player, {Number, proplists:get_value(Number, VisitPlayers, <<"N/N">>)}} |
             lists:keydelete(player, 1, Data)];
        {undefined, HomeId, _} ->
          [{player, {Number, proplists:get_value(Number, HomePlayers, <<"N/N">>)}},
           {team, HomeId} | lists:keydelete(player, 1, Data)]
      end
  end;
parse(MatchId, substitution, Data) ->
  case proplists:get_value(player_out, Data) of
    {_, _} -> Data;
    undefined -> Data;
    Number ->
      #match_stream_match{home_players = HomePlayers,
                          home = Home, visit = Visit,
                          visit_players = VisitPlayers} =
                           match_stream_db:match(MatchId),
      case {proplists:get_value(team, Data),
            Home#match_stream_team.team_id,
            Visit#match_stream_team.team_id} of
        {HomeId, HomeId, _} ->
          [{player_out, {Number, proplists:get_value(Number, HomePlayers, <<"N/N">>)}} |
             lists:keydelete(player_out, 1, Data)];
        {VisitId, _, VisitId} ->
          [{player_out, {Number, proplists:get_value(Number, VisitPlayers, <<"N/N">>)}} |
             lists:keydelete(player_out, 1, Data)];
        {undefined, HomeId, _} ->
          [{player_out, {Number, proplists:get_value(Number, HomePlayers, <<"N/N">>)}},
           {team, HomeId} | lists:keydelete(player_out, 1, Data)]
      end
  end;
parse(_, _, Data) -> Data.