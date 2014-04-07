-define(LOG(Format, Args, Level, Tags), error_logger:info_msg("~p:~p " ++ Format,[?MODULE,?LINE] ++ Args)).

-define(LOGGER_STUB_3, fun(_,_,_) -> true end).
-define(LOGGER_STUB_2, fun(_,_) -> true end).
-define(LOGGER_STUB_1, fun(_) -> true end).


-ifdef(SERVER_LOG_VERBOSE).
-ifdef(SERVER_LOG_DEBUG).

-define(DBG(Format, Args, Tag), ?LOG(Format, Args, ?debug, Tag)).
-define(DBG(Format, Args),      ?DBG(Format, Args, [])).
-define(DBG(Format),            ?DBG(Format, [])).

-else. %% LOG_DEBUG

-define(DBG(Format, Args, Tag), ?LOGGER_STUB_3(Format, Args, Tag)).
-define(DBG(Format, Args),      ?LOGGER_STUB_2(Format, Args)).
-define(DBG(Format),            ?LOGGER_STUB_1(Format)).

-endif. %% LOG_DEBUG

-define(INFO(Format, Args, Tag), ?LOG(Format, Args, ?info, Tag)).
-define(INFO(Format, Args),      ?INFO(Format, Args, [])).
-define(INFO(Format),            ?INFO(Format, [])).

-else. %% LOG_VERBOSE

-define(INFO(Format, Args, Tag), ?LOGGER_STUB_3(Format, Args, Tag)).
-define(INFO(Format, Args),      ?LOGGER_STUB_2(Format, Args)).
-define(INFO(Format),            ?LOGGER_STUB_1(Format)).

-endif. %% LOG_VERBOSE

-define(NOTICE(Format, Args, Tag), ?LOG(Format, Args, ?notice, Tag)).
-define(NOTICE(Format, Args),      ?NOTICE(Format, Args, [])).
-define(NOTICE(Format),            ?NOTICE(Format, [])).

-define(WARNING(Format, Args, Tag), ?LOG(Format, Args, ?warning, Tag)).
-define(WARNING(Format, Args),      ?WARNING(Format, Args, [])).
-define(WARNING(Format),            ?WARNING(Format, [])).

-define(ERROR(Format, Args, Tag), ?LOG(Format, Args, ?error, Tag)).
-define(ERROR(Format, Args),      ?ERROR(Format, Args, [])).
-define(ERROR(Format),            ?ERROR(Format, [])).

-define(CRITICAL(Format, Args, Tag), ?LOG(Format, Args, ?critical, Tag)).
-define(CRITICAL(Format, Args),      ?CRITICAL(Format, Args, [])).
-define(CRITICAL(Format),            ?CRITICAL(Format, [])).

-define(ALERT(Format, Args, Tag), ?LOG(Format, Args, ?alert, Tag)).
-define(ALERT(Format, Args),      ?ALERT(Format, Args, [])).
-define(ALERT(Format),            ?ALERT(Format, [])).

-define(EMERGENCY(Format, Args, Tag), ?LOG(Format, Args, ?emergency, Tag)).
-define(EMERGENCY(Format, Args),      ?EMERGENCY(Format, Args, [])).
-define(EMERGENCY(Format),            ?EMERGENCY(Format, [])).
