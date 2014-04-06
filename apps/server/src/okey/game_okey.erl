-module(game_okey).
-export([get_player_stats/1]).
-include_lib("server/include/basic_types.hrl").
-include_lib("server/include/game_okey.hrl").

get_player_stats(PlayerId) ->
    {ok, GameStats} = game_stats:get_game_points(okey, PlayerId),
    {ok, Skill} = game_stats:get_skill(PlayerId),
    {ok, PlayerStats} = game_stats:get_player_stats(PlayerId),
    #'PlayerOkeyStats'{playerId = PlayerId,
                       level = Skill,
                       score = proplists:get_value(game_points, GameStats),
                       numberOkey = proplists:get_value(finished_with_okey, GameStats),
                       number8Tashes = proplists:get_value(finished_with_8_tashes, GameStats),
                       totalWins = proplists:get_value(total_wins, PlayerStats),
                       totalLose = proplists:get_value(total_loses, PlayerStats),
                       totalDisconnects = proplists:get_value(total_disconnects, PlayerStats),
                       overalSuccessRatio = proplists:get_value(overall_success_ratio, PlayerStats),
                       averagePlayDuration = proplists:get_value(average_play_time, PlayerStats)
                      }.
