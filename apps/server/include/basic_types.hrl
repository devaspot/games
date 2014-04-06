-ifndef(BASIC_TYPES).
-define(BASIC_TYPES, true).
-type 'tag'()        :: any().
-type 'PlayerId'()   :: any().
-type 'GameId'()     :: any().
-type 'ChatId'()     :: any().
-type 'SocialActionEnum'() :: integer().
%-type proplist()     :: list(tuple(atom(), any())).
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
