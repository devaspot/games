
-ifndef(TRANSACTION_HRL).
-define(TRANSACTION_HRL, "transaction_hrl").

-include_lib("kvs/include/kvs.hrl").

-type currency() :: kakaush | quota | game_point | money.

-record(transaction, {?ITERATOR(feed),
        timestamp :: erlang:now(),
        amount :: integer(),
        currency :: currency(),
        comment,
        info}).

-endif.
