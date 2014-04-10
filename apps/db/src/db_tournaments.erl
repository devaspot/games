-module(db_tournaments).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/tournaments.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = kvs, tables = 
                [
                 #table{name = team, fields=record_info(fields, team)},
                 #table{name = tournament, fields=record_info(fields, tournament)},
                 #table{name = play_record, fields=record_info(fields, play_record)}
                ]
           }.
