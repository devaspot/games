-module(gs).
-compile(export_all).

info(String, Args) ->  error_logger:info_msg(String, Args).
info(String) -> error_logger:info_msg(String).
warning(String, Args) -> error_logger:warning_msg(String, Args).
warning(String) -> error_logger:warning_msg(String).
error(String, Args) -> error_logger:error_msg(String, Args).
error(String) -> error_logger:error_msg(String).
