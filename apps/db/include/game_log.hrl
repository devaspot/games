-ifndef(GAME_LOG_HRL).
-define(GAME_LOG_HRL, "game_log.hrl").

-include_lib("kvs/include/kvs.hrl").

-record(game_log,  {?CONTAINER, protocol_stat=[] }).
-record(reveal_log,  {?CONTAINER, reveal_stat=[] }).
-record(event_log, {?ITERATOR(game_log),
    game_id,
    timestamp,
    user,
    event,
    game_event}).

-endif.
