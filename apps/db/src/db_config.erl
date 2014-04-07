-module(db_config).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/config.hrl").
-compile(export_all).

metainfo() ->
    #schema{name=kvs,tables=[
        #table{name=config,fields=record_info(fields,config)}
    ]}.
