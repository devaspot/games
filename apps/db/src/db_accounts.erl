-module(db_accounts).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/transaction.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = kvs, tables = 
                [
                 #table{name = transaction, fields=record_info(fields, transaction)}
                ]
           }.
