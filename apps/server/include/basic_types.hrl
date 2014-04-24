-ifndef(BASIC_TYPES).
-define(BASIC_TYPES, true).

-type 'PlayerId'()   :: any() | 0.
-type 'GameId'()     :: any().
-type 'SocialActionEnum'() :: integer().
-type 'MonitorRef'() :: reference().

-record('PlayerInfo', {
          id         :: 'PlayerId'(),
          login      :: string(),
          name       :: string(),
          surname    :: string(),
          age        :: integer(),
          skill :: integer(),
          score :: integer(),
          avatar_url :: string(),
          robot = false :: boolean()
         }).

-endif.
