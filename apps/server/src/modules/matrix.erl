%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The interface to the elimination tournaments plans matrix.
%%%
%%% Created : Feb 15, 2013
%%% -------------------------------------------------------------------

-module(matrix).

-export([get_plan/4,
         get_prize_fund/4,
         get_plan_desc/4,
         get_tours/3]).

get_plan(GameType, KakushPerRound, RegistrantsNum, Tours) ->
    case lists:keyfind({KakushPerRound, RegistrantsNum, Tours}, 1, tournament_matrix(GameType)) of
        false -> {error, no_such_plan};
        {_NQ, _K, Plan, _String} -> {ok, Plan}
    end.

get_prize_fund(GameType, KakushPerRound, RegistrantsNum, Tours) ->
    case lists:keyfind({KakushPerRound, RegistrantsNum, Tours}, 1, tournament_matrix(GameType)) of
        false -> {error, no_such_plan};
        {_NQ, K, _Plan, _String} -> {ok, K}
    end.

get_plan_desc(GameType, KakushPerRound, RegistrantsNum, Tours) ->
    case lists:keyfind({KakushPerRound, RegistrantsNum, Tours}, 1, tournament_matrix(GameType)) of
        false -> {error, no_such_plan};
        {_NQ, _K, _Plan, String} -> {ok, String}
    end.

get_tours(GameType, KakushPerRound, RegistrantsNum) ->
    [T || {{Q, N, T}, _, _, _String} <- tournament_matrix(GameType), Q==KakushPerRound, N==RegistrantsNum].

tournament_matrix(game_okey) ->
    [%% Quota Pl.No  Fund   1          2         3         4         5         6         7         8
     { {  8,   16,3}, 54,   [ne      , {ce,  4}, {te,  1}                                                  ], ["yok","1.ler","Final"]},
     { { 10,   16,3}, 72,   [ne      , {ce,  4}, {te,  1}                                                  ], ["yok","1.ler","Final"]},
     { {  2,   64,4}, 80,   [ne      , {ce, 16}, {te,  1}, {te,  1}                                        ], ["yok","yok","1.ler","Final"]},
     { {  4,   64,4}, 98,   [ne      , {ce, 16}, {te,  1}, {te,  1}                                        ], ["yok","yok","1.ler","Final"]},
     { {  6,   64,4}, 158,  [ne      , {ce, 16}, {te,  1}, {te,  1}                                        ], ["yok","yok","1.ler","Final"]},
     { {  8,   64,4}, 223,  [ne      , {ce, 16}, {te,  1}, {te,  1}                                        ], ["yok","yok","1.ler","Final"]},
     { { 10,   64,4}, 295,  [ne      , {ce, 16}, {te,  1}, {te,  1}                                        ], ["yok","yok","1.ler","Final"]},
     { {  2,  128,5}, 81,   [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}                              ], ["yok","yok","1.ler","Final"]},
     { {  4,  128,5}, 162,  [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}                              ], []},
     { {  6,  128,5}, 260,  [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}                              ], []},
     { {  8,  128,5}, 368,  [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}                              ], []},
     { { 10,  128,5}, 487,  [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}                              ], []},
     { {  2,  256,5}, 198,  [ne      , {ce, 64}, {te,  1}, {te,  1}, {te,  1}                              ], []},
     { {  4,  256,5}, 397,  [ne      , {ce, 64}, {te,  1}, {te,  1}, {te,  1}                              ], []},
     { {  6,  256,5}, 635,  [ne      , {ce, 64}, {te,  1}, {te,  1}, {te,  1}                              ], []},
     { {  8,  256,5}, 899,  [ne      , {ce, 64}, {te,  1}, {te,  1}, {te,  1}                              ], []},
     { { 10,  256,5}, 1190, [ne      , {ce, 64}, {te,  1}, {te,  1}, {te,  1}                              ], []},
     { {  2,  256,7}, 283,  [ne      , {ce,128}, ne      , {ce, 64}, {te,  1}, {te,  1}, {te,   1}         ], []},
     { {  4,  256,7}, 566,  [ne      , {ce,128}, ne      , {ce, 64}, {te,  1}, {te,  1}, {te,   1}         ], []},
     { {  6,  256,7}, 907,  [ne      , {ce,128}, ne      , {ce, 64}, {te,  1}, {te,  1}, {te,   1}         ], []},
     { {  8,  256,7}, 1285, [ne      , {ce,128}, ne      , {ce, 64}, {te,  1}, {te,  1}, {te,   1}         ], []},
     { { 10,  256,7}, 1701, [ne      , {ce,128}, ne      , {ce, 64}, {te,  1}, {te,  1}, {te,   1}         ], []},
     { {  2,  512,6}, 326,  [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","ilk 2","ilk 2","ilk 2","1.ler","Final"]},
     { {  4,  512,6}, 652,  [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","ilk 2","ilk 2","ilk 2","1.ler","Final"]},
     { {  6,  512,6}, 1043, [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","ilk 2","ilk 2","ilk 2","1.ler","Final"]},
     { {  8,  512,6}, 1478, [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","ilk 2","ilk 2","ilk 2","1.ler","Final"]},
     { { 10,  512,6}, 1957, [{te,  2}, {te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","ilk 2","ilk 2","ilk 2","1.ler","Final"]},
     { {  2,  512,8}, 582,  [ne      , {ce,256}, ne      , {ce,128}, {te,  2}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  4,  512,8}, 1163, [ne      , {ce,256}, ne      , {ce,128}, {te,  2}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  6,  512,8}, 1861, [ne      , {ce,256}, ne      , {ce,128}, {te,  2}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  8,  512,8}, 2637, [ne      , {ce,256}, ne      , {ce,128}, {te,  2}, {te,  1}, {te,  1}, {te,  1}], []},
     { { 10,  512,8}, 3490, [ne      , {ce,256}, ne      , {ce,128}, {te,  2}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  2, 1024,6}, 795,  [ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], []},
     { {  4, 1024,6}, 1589, [ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], []},
     { {  6, 1024,6}, 2543, [ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], []},
     { {  8, 1024,6}, 3602, [ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], []},
     { { 10, 1024,6}, 4767, [ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], []},
     { {  2, 1024,8}, 1135, [ne      , {ce,512}, ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  4, 1024,8}, 2271, [ne      , {ce,512}, ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  6, 1024,8}, 3633, [ne      , {ce,512}, ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  8, 1024,8}, 5147, [ne      , {ce,512}, ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { { 10, 1024,8}, 6812, [ne      , {ce,512}, ne      , {ce,256}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  2, 2048,6}, 1135, [{te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","1.ler","1.ler","1.ler","1.ler","Final"]},
     { {  4, 2048,6}, 2271, [{te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","1.ler","1.ler","1.ler","1.ler","Final"]},
     { {  6, 2048,6}, 3633, [{te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","1.ler","1.ler","1.ler","1.ler","Final"]},
     { {  8, 2048,6}, 5147, [{te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","1.ler","1.ler","1.ler","1.ler","Final"]},
     { { 10, 2048,6}, 6812, [{te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}, {te,  1}                    ], ["ilk 2","1.ler","1.ler","1.ler","1.ler","Final"]},
     { {  2, 2048,8}, 1987, [ne      , {ce,1024},{te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  4, 2048,8}, 3974, [ne      , {ce,1024},{te,  2}, {te,  2}, {te,  1}, {te,  1}, {te,  1}, {te,  1}], []},
     { {  2, 1020,1}, 9359, [{te,1}                                                                        ], []},
     { {  4, 1020,1}, 8359, [{te,1}                                                                        ], []},
     { {  6, 1020,1}, 7359, [{te,1}                                                                        ], []},
     { {  8, 1020,1}, 6359, [{te,1}                                                                        ], []},
     { { 10, 1020,1}, 5359, [{te,1}                                                                        ], []},
     { {  2, 1020,2}, 5959, [ne      , {te,1}                                                              ], []},
     { {  4, 1020,2}, 5859, [ne      , {te,1}                                                              ], []},
     { {  6, 1020,2}, 5759, [ne      , {te,1}                                                              ], []},
     { {  8, 1020,2}, 5659, [ne      , {te,1}                                                              ], []},
     { { 10, 1020,2}, 5559, [ne      , {te,1}                                                              ], []}, 
     { {  2, 1020,3}, 4359, [ne      , ne       , {te,1}                                                   ], []},
     { {  4, 1020,3}, 4359, [ne      , ne       , {te,1}                                                   ], []},
     { {  6, 1020,3}, 4359, [ne      , ne       , {te,1}                                                   ], []},
     { {  8, 1020,3}, 4359, [ne      , ne       , {te,1}                                                   ], []},
     { { 10, 1020,3}, 4359, [ne      , ne       , {te,1}                                                   ], []},
     { {  2, 1020,4}, 3359, [ne      , ne       , ne     , {te,1}                                          ], []},
     { {  4, 1020,4}, 3359, [ne      , ne       , ne     , {te,1}                                          ], []},
     { {  6, 1020,4}, 3359, [ne      , ne       , ne     , {te,1}                                          ], []},
     { {  8, 1020,4}, 3359, [ne      , ne       , ne     , {te,1}                                          ], []},
     { {  2, 1020,6}, 2359, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { {  4, 1020,6}, 2359, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { {  6, 1020,6}, 2359, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { {  2,  500,1}, 1359, [{te,1}                                                                        ], []},
     { {  4,  500,2}, 5959, [ne      , {te,1}                                                              ], []},
     { {  6,  500,3}, 4859, [ne      , ne       , {te,1}                                                   ], []},
     { {  8,  500,4}, 3759, [ne      , ne       , ne     , {te,1}                                          ], []},
     { { 10,  500,6}, 2359, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { {  2,  400,1}, 4359, [{te,1}                                                                        ], []},
     { {  4,  400,2}, 3359, [ne      , {te,1}                                                              ], []},
     { {  6,  400,3}, 2359, [ne      , ne       , {te,1}                                                   ], []},
     { {  8,  400,4}, 1359, [ne      , ne       , ne     , {te,1}                                          ], []},
     { { 10,  400,6}, 3359, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { {  2,  300,1}, 2359, [{te,1}                                                                        ], []},
     { {  4,  300,2}, 1359, [ne      , {te,1}                                                              ], []},
     { {  6,  300,3}, 0959, [ne      , ne       , {te,1}                                                   ], []},
     { {  8,  300,4}, 0359, [ne      , ne       , ne     , {te,1}                                          ], []},
     { { 10,  300,6}, 1959, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { {  2,  200,1}, 0859, [{te,1}                                                                        ], []},
     { {  4,  200,2}, 0759, [ne      , {te,1}                                                              ], []},
     { {  6,  200,3}, 0659, [ne      , ne       , {te,1}                                                   ], []},
     { {  8,  200,4}, 0559, [ne      , ne       , ne     , {te,1}                                          ], []},
     { { 10,  200,6}, 0459, [ne      , ne       , ne     , ne      , ne      , {te,1}                      ], []},
     { { 10,   40,2}, 0459, [ne      , {te,1}                                                              ], []}
  ];

tournament_matrix(game_tavla) ->
    [%% Quota Pl.No  Fund    1          2         3         4         5         6         7         8
     { { 10,   16,4}, 54,   [{te,  1} , {te,  1}, {te,  1}, {te,  1}                                       ], ["1.ler","1.ler","1.ler","Final"]}
    ].

