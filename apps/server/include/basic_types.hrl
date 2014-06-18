-ifndef(BASIC_TYPES).
-define(BASIC_TYPES, true).

-type 'PlayerId'()   :: any().
-type 'GameId'()     :: any().

-record('PlayerInfo', {
    id :: 'PlayerId'(),
    login :: string(),
    name :: string(),
    surname :: string(),
    seat_num,
    connected,
    age :: integer(),
    skill :: integer(),
    score :: integer(),
    avatar_url :: string(),
    robot = false :: boolean(),
    sex }).

-endif.
