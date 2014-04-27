-module(db_game_log).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/game_log.hrl").
-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=game_log,container=true,fields=record_info(fields,game_log)},
        #table{name=reveal_log,container=true,fields=record_info(fields,reveal_log)},
        #table{name=event_log,container=game_log,fields=record_info(fields,event_log)}
    ]}.
