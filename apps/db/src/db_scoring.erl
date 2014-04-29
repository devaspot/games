-module(db_scoring).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("db/include/scoring.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = kvs, tables = [
        #table{name = pointing_rule,  fields=record_info(fields, pointing_rule)}]}.

setup() ->

    %% OKEY

    Rounds5 = #pointing_rule{rounds = 5, game = okey, kakush_winner = 15, kakush_other = 3, quota = 5},
    kvs:put(Rounds5#pointing_rule{id = {okey, standard, 5}, game_type = standard, game_points = 1}),
    kvs:put(Rounds5#pointing_rule{id = {okey, evenodd,  5}, game_type = evenodd,  game_points = 5}),
    kvs:put(Rounds5#pointing_rule{id = {okey, color,    5}, game_type = color,    game_points = 7}),

    Rounds10 = #pointing_rule{rounds = 10, game = okey, kakush_winner = 15, kakush_other = 3, quota = 10},
    kvs:put(Rounds10#pointing_rule{id = {okey, standard, 10}, game_type = standard, game_points = 7}),
    kvs:put(Rounds10#pointing_rule{id = {okey, evenodd,  10}, game_type = evenodd,  game_points = 10}),
    kvs:put(Rounds10#pointing_rule{id = {okey, color,    10}, game_type = color,    game_points = 15}),

    Rounds20 = #pointing_rule{rounds = 20, game = okey, kakush_winner = 15, kakush_other = 3, quota = 20},
    kvs:put(Rounds20#pointing_rule{id = {okey, standard, 20}, game_type = standard, game_points = 15}),
    kvs:put(Rounds20#pointing_rule{id = {okey, evenodd,  20}, game_type = evenodd,  game_points = 20}),
    kvs:put(Rounds20#pointing_rule{id = {okey, color,    20}, game_type = color,    game_points = 30}),

    Rounds40 = #pointing_rule{rounds = 40, game = okey, kakush_winner = 32, kakush_other = 7, quota = 40},
    kvs:put(Rounds40#pointing_rule{id = {okey, standard, 40}, game_type = standard, game_points = 30}),
    kvs:put(Rounds40#pointing_rule{id = {okey, evenodd,  40}, game_type = evenodd,  game_points = 40}),
    kvs:put(Rounds40#pointing_rule{id = {okey, color,    40}, game_type = color,    game_points = 60}),

    Rounds60 = #pointing_rule{rounds = 60, game = okey, kakush_winner = 32, kakush_other = 7, quota = 40},
    kvs:put(Rounds60#pointing_rule{id = {okey, standard, 60}, game_type = standard, game_points = 60}),
    kvs:put(Rounds60#pointing_rule{id = {okey, evenodd,  60}, game_type = evenodd,  game_points = 80}),
    kvs:put(Rounds60#pointing_rule{id = {okey, color,    60}, game_type = color,    game_points = 120}),

    Rounds80 = #pointing_rule{rounds = 80, game = okey, kakush_winner = 70, kakush_other = 15, quota = 80},
    kvs:put(Rounds80#pointing_rule{id = {okey, standard, 80}, game_type = standard, game_points = 120}),
    kvs:put(Rounds80#pointing_rule{id = {okey, evenodd,  80}, game_type = evenodd,  game_points = 160}),
    kvs:put(Rounds80#pointing_rule{id = {okey, color,    80}, game_type = color,    game_points = 240}),

    %% Countdown 10
    kvs:put(#pointing_rule{id = {okey, countdown}, game = okey, game_type = countdown,
         kakush_winner = 6, kakush_other = 1, quota = 8, game_points = 10}),

    %% Feel lucky
    kvs:put(#pointing_rule{id = {okey, feellucky}, game = okey, game_type = feellucky,
         kakush_winner = 0, kakush_other = 0, quota = 0, game_points = 0}),

    %% TAVLA

    GameTypes = [standard, evenodd],
    %% here we store points in rounds field
    [kvs:put(#pointing_rule{id = {tavla, GT, 3}, rounds = 3, game = tavla, game_type = GT,
          kakush_winner = 1, kakush_other = 1, quota = 3, game_points = 5})
       || GT <- GameTypes],


    [kvs:put(#pointing_rule{id = {tavla, GT, 5}, rounds = 5, game = tavla, game_type = GT,
          kakush_winner = 2, kakush_other = 1, quota = 4, game_points = 5})
       || GT <- GameTypes],

    [kvs:put(#pointing_rule{id = {tavla, GT, 7}, rounds = 7, game = tavla, game_type = GT,
          kakush_winner = 3, kakush_other = 1, quota = 5, game_points = 7})
       || GT <- GameTypes],

    %% Kakara

    kvs:put(#pointing_rule{id = {tavla, kakara}, game = tavla, game_type = kakara,
         kakush_winner = 0, kakush_other = 0, quota = 1, game_points = 0}).

