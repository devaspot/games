-ifndef(JOURNAL_HRL).
-define(JOURNAL_HRL, "journal.hrl").

-include_lib("kvs/include/kvs.hrl").
-define(LOG_HEADER, game_id, date, time, user, module, type, speed, rounds).
-define(CONTAINER_LOG, ?CONTAINER, ?LOG_HEADER, stats=[]).
-record(container_log, {?CONTAINER_LOG}).
-record(container_event, {?ITERATOR(container_log), ?LOG_HEADER }).

-record(series_log,  {?CONTAINER_LOG, score, game_points, kakush }).
-record(series_event,  {?ITERATOR(series_log),?LOG_HEADER, result, score}).

-record(reveal_log,  {?CONTAINER_LOG, skill, score }).
-record(reveal_event,  {?ITERATOR(reveal_log), ?LOG_HEADER, reason, winner, score, total}).

-record(protocol_log,  {?CONTAINER_LOG}).
-record(protocol_event, {?ITERATOR(protocol_log), ?LOG_HEADER, event, game_event}).

-endif.
