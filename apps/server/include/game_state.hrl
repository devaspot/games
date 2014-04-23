-define(PLAYER,
         id              :: pos_integer(), %% Player Id
         seat_num        :: integer(),
         user_id         :: binary(),
         is_bot          :: boolean(),
         info            :: #'PlayerInfo'{},
         connected       :: boolean()
        ).

-define(GAME_STATE, 
         game_id              :: pos_integer(),
         table_id             :: pos_integer(),
         table_name           :: string(),
         players,             %% The register of table players
         parent               :: {atom(), pid()},
         parent_mon_ref       :: reference(),
         relay                :: pid(),
         mult_factor          :: integer(),
         slang_flag           :: boolean(),
         observer_flag        :: boolean(),
         tournament_type      :: atom(), %%  standalone | elimination | pointing | lucky
         speed                :: slow | normal | fast,
         game_mode            :: standard | color | evenodd | countdown,
         social_actions_enabled :: boolean(),
         tournament_table     :: list(), %% [{TurnNum, TurnRes}], TurnRes = [{PlayerId, Points, Status}]
         % game tree
         rounds               :: undefined | integer(), %% Not defined for countdown game type
         tour                 :: undefined | integer(),
         tours                :: undefined | integer(),
         cur_round            :: integer(),
         next_series_confirmation :: yes_exit | no_exit | no,
         % timeouts
         turn_timeout         :: integer(),
         ready_timeout        :: integer(),
         round_timeout        :: infinity | integer(),
         set_timeout          :: infinity | integer(),
         timeout_timer        :: undefined | reference(),
         timeout_magic        :: term(),
         round_timer          :: undefined | reference(),
         set_timer            :: undefined | reference(),
         wait_list            :: list(),
         % pause
         pause_mode           :: disabled | normal,
         paused_statename     :: atom(), %% For storing a statename before pause
         paused_timeout_value :: integer(), %% For storing remain timeout value
         % scoring
         scoring_state        :: term()).

-record(table_state, {?GAME_STATE}).

-record(okey_state, {?GAME_STATE,
         reveal_confirmation_timeout    :: integer(),
         reveal_confirmation  :: boolean(),
         gosterge_finish_allowed :: undefined | boolean(), %% Only defined for countdown game type
         %% Dynamic parameters
         desk_rule_pid        :: undefined | pid(),
         start_seat           :: integer(), %% The player who moves first
         reveal_confirmation_list :: list(), %% {SeatNum, Answer}
         desk_state %% OKEY DESK
        }).

-record(tavla_state, {?GAME_STATE,
         tables_num           :: integer(), %% For paired mode >= 1, otherwise  = 1
         %% Dynamic parameters
         desk_rule_pid        :: undefined | pid(),
         start_color, %% The player who moves first
         desk_state %% TAVLA DESK
        }).
