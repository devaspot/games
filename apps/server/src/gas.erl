-module(gas).
-compile(export_all).

%-define(ALLOWED, [gas,nsg_trn_lucky,game_session,game_manager,game_okey_ng_table_trn]).
-define(ALLOWED, [gas,auth_server]).

log(Module, String, Args, Fun) ->
    case lists:member(Module,?ALLOWED) of
         true -> error_logger:Fun(String, Args);
         false -> skip end.

info(Module,String, Args) ->  log(Module,String, Args, info_msg).
info(String, Args) -> log(?MODULE, String, Args, info_msg).
info(String) -> log(?MODULE, String, [], info_msg).

warning(Module,String, Args) -> log(Module, String, Args, warning_msg).
warning(String, Args) -> log(?MODULE, String, Args, warning_msg).
warning(String) -> log(?MODULE,String, [], warning_msg).

error(Module,String, Args) -> log(Module, String, Args, error_msg).
error(String, Args) -> log(?MODULE, String, Args, error_msg).
error(String) -> log(?MODULE, String, [], error_msg).
