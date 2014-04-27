-module(db_game_log).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/game_log.hrl").
-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=series_log,container=true,fields=record_info(fields,series_log),keys=[?LOG_HEADER]},
        #table{name=reveal_log,container=true,fields=record_info(fields,reveal_log),keys=[?LOG_HEADER]},
        #table{name=protocol_log,container=true,fields=record_info(fields,protocol_log),keys=[?LOG_HEADER]},
        #table{name=series_event,container=series_log,fields=record_info(fields,series_event),keys=[?LOG_HEADER]},
        #table{name=reveal_event,container=reveal_log,fields=record_info(fields,reveal_event),keys=[?LOG_HEADER]},
        #table{name=protocol_event,container=protocol_log,fields=record_info(fields,protocol_event),keys=[?LOG_HEADER]}
    ]}.
