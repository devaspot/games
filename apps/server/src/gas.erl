-module(gas).
-compile(export_all).

-define(ALLOWED, [nsg_trn_lucky,game_session,game_manager,game_okey_ng_table_trn]).

info(Module,String, Args) ->
    case lists:member(Module,?ALLOWED) of
         true -> error_logger:info_msg(String, Args);
         false -> skip end.

info(String, Args) ->  error_logger:info_msg(String, Args).
info(String) -> error_logger:info_msg(String).
warning(Module,String, Args) -> error_logger:warning_msg(String, Args).
warning(String, Args) -> error_logger:warning_msg(String, Args).
warning(String) -> error_logger:warning_msg(String).
error(Module,String, Args) -> error_logger:error_msg(String, Args).
error(String, Args) -> error_logger:error_msg(String, Args).
error(String) -> error_logger:error_msg(String).
