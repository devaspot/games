-ifndef(TABLE_HRL).
-define(TABLE_HRL, "table.hrl").

-include_lib("kvs/include/kvs.hrl").

-record(game_table, {?ITERATOR(feed),
    name,
    gameid,
    trn_id,
    game_type,
    rounds,
    sets,
    owner,
    timestamp,
    users = [],
    users_options = [],
    game_mode,
    game_options,
    game_speed,
    friends_only,
    invited_users = [],
    private :: boolean(),
    feel_lucky = false :: boolean(),
    creator,
    age_limit,
    groups_only = [],
    gender_limit,
    location_limit = "",
    paid_only,
    deny_robots = false :: boolean(),
    slang,
    deny_observers,
    gosterge_finish = false :: boolean(),
    double_points = 1 :: integer(),
    game_state,
    game_process :: pid(),
    game_module :: atom(),
    pointing_rules :: any(),
    pointing_rules_ex :: [], %% [#pointing_rule{}] - list of additional pointing rules,
    game_process_monitor :: reference(),
    tournament_type = simple :: simple | paired | paired_lobby | tournament,
    robots_replacement_allowed = true :: boolean()}).

-endif.
