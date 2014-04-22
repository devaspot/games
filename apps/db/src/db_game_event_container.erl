-module(db_game_event_container).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/game_event_container.hrl").
-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=game_event_container,fields=record_info(fields,game_event_container)}
    ]}.
