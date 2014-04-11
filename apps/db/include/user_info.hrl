-ifndef(USER_INFO_HRL).
-define(USER_INFO_HRL, "user_info.hrl").

-include("../../server/include/types.hrl").

-record(user_info,
        {username :: username_type(),
         name,
         surname,
         age,
         avatar_url,
         sex,
         skill = 0 :: integer(),
         score = 0 :: integer()}).

-endif.
