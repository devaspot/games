-ifndef(SCORING_HRL).
-define(SCORING_HRL, "scoring_hrl").

-include_lib("kvs/include/kvs.hrl").

-record(ti_game_event, {
        id                           :: integer(),         %% GameId
        type,
        game_name,
        game_mode,
        double_points,
        tournament_type = standalone }).

-record(pointing_rule, {?ITERATOR(feed),
        pointing_rule_id,            %% {Game, GameType, Rounds} | {Game, GameType}
        game,
        game_type,
        rounds,        %% rounds | points | undefined
        kakush_winner, %% kakush points will be assigned for the winner of the game.
        kakush_other,  %% kakush points will be assigned for the rest of the
        quota,
        game_points}).

-endif.
