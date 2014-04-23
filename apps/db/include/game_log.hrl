-ifndef(GAME_LOG_HRL).
-define(GAME_LOG_HRL, "game_log.hrl").

-include_lib("kvs/include/kvs.hrl").

-record(event_log, {?ITERATOR(feed),
    game_id,
    timestamp,
    user,
    event,
    game_event}).

-record(game_log, {?ITERATOR(feed),
    game_type,
    game_id,
    user,
    aggregations % [{attach,Count},{join,Count},{}]
    }).

-endif.
