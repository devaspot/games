-module(db_accounts).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/accounts.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = kvs, tables = 
                [
                 #table{name = account, fields=record_info(fields, account)},
                 #table{name = pointing_rule, fields=record_info(fields, pointing_rule)}
                ]
           }.
