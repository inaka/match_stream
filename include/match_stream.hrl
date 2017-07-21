-include("elog.hrl").

-record(match_stream_event, {timestamp  :: pos_integer(),
                             match_id   :: match_stream:match_id(),
                             kind       :: match_stream:event_kind(),
                             data       :: [match_stream:data()]}).

-record(match_stream_user, {user_id :: match_stream:user_id(),
                            visit_count = 1 :: pos_integer()}).

-record(match_stream_team, {team_id   :: match_stream:team_id(),
                            name      :: binary(),
                            division  :: match_stream:division()}).

-record(match_stream_match, {match_id             :: match_stream:match_id(),
                             home                 :: match_stream:team(),
                             visit                :: match_stream:team(),
                             start_time           :: match_stream:date(),
                             home_players         :: [match_stream:player()],
                             visit_players        :: [match_stream:player()],
                             home_score = 0       :: non_neg_integer(),
                             visit_score = 0      :: non_neg_integer(),
                             period = not_started :: match_stream:period(),
                             stadium              :: binary(),
                             period_start         :: undefined | pos_integer()}).
