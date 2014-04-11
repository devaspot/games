-module(db_table).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/table.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = kvs, tables = 
                [
                 #table{name = game_table, fields=record_info(fields, game_table)}
                ]
           }.
