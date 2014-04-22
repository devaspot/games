-ifndef(GAME_EVENT_CONTAINER_HRL).
-define(GAME_EVENT_CONTAINER_HRL, "game_event_container.hrl").

-include_lib("kvs/include/kvs.hrl").

-record(game_event_container,
        {
          ?ITERATOR(feed),
          time,
          game_event
        }).

-endif.
