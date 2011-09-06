-record(match_stream_event, {match_id  :: match_stream:match_id(),
                             kind      :: match_stream:event_kind(),
                             data      :: [{atom(), binary()}]}).