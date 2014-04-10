-module(db_scoring).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/scoring.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = kvs, tables = 
                [
                 #table{name = player_scoring, fields=record_info(fields, player_scoring)},
                 #table{name = scoring_record, fields=record_info(fields, scoring_record)},
                 #table{name = personal_score, fields=record_info(fields, personal_score)}
                ]
           }.
