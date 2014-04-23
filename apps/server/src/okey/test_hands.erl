%%----------------------------------------------------------------------
%% @author Yura Zhloba <yzh44yzh@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% This module allows to set up pieces for next game
%% @end
%%----------------------------------------------------------------------

-module(test_hands).

-include_lib("server/include/requests.hrl").
-include_lib("server/include/game_okey.hrl").

-include_lib("eunit/include/eunit.hrl").

-export([set_pieces/1, clear/0]).
-export([pieces_set_1/0, pieces_set_2/0]).

-spec set_pieces(list() | undefined) -> ok.
set_pieces(P) ->
    gas:info(?MODULE,"pieces ~p", [P]),
    A = kakaconfig:set([games, okey, debug_next_round_pieces], P),
    gas:info(?MODULE,"set result: ~p", [A]),
    ok.

-spec clear() -> ok.
clear() ->
    set_pieces(undefined).

set_1() ->
    A = to_pieces([
                   {1,1}, {2,1}, {3,1}, {4,1},
                   {1,2}, {2,2}, {3,2}, {4,2},
                   {1,3}, {2,3}, {3,3}, {4,3},
                   {1,4}, {2,4}, {3,4}, {4,4},
                   {1,5}, {2,5}, {3,5}, {4,5},
                   {1,6}, {2,6}, {3,6}, {4,6},
                   {1,7}, {2,7}, {3,7}, {4,7},
                   {1,8}, {2,8}, {3,8}, {4,8},
                   {1,9}, {2,9}, {3,9}, {4,9},
                   {1,10}, {2,10}, {3,10}, {4,10},
                   {1,11}, {2,11}, {3,11}, {4,11},
                   {1,12}, {2,12}, {3,12}, {4,12},
                   {1,13}, {2,13}, {3,13}, {4,13},
                   {1,1}, {2,1}, {3,1}, {4,1},
                   {1,2}, {2,2}, {3,2}, {4,2},
                   {1,3}, {2,3}, {3,3}, {4,3},
                   {1,4}, {2,4}, {3,4}, {4,4},
                   {1,5}, {2,5}, {3,5}, {4,5},
                   {1,6}, {2,6}, {3,6}, {4,6},
                   {1,7}, {2,7}, {3,7}, {4,7},
                   {1,8}, {2,8}, {3,8}, {4,8},
                   {1,9}, {2,9}, {3,9}, {4,9},
                   {1,10}, {2,10}, {3,10}, {4,10},
                   {1,11}, {2,11}, {3,11}, {4,11},
                   {1,12}, {2,12}, {3,12}, {4,12},
                   {1,13}, {2,13}, {3,13}, {4,13}
                  ]),
    A ++ [?FALSE_OKEY, ?FALSE_OKEY].

set_2() ->
    A = to_pieces([
                   {2,1}, {4,1}, {3,1}, {1,1},
                   {2,2}, {4,2}, {3,2}, {1,2},
                   {2,3}, {4,3}, {3,3}, {1,3},
                   {2,4}, {4,4}, {3,4}, {1,4},
                   {2,5}, {4,5}, {3,5}, {1,5},
                   {2,6}, {4,6}, {3,6}, {1,6},
                   {2,7}, {4,7}, {3,7}, {1,7},
                   {2,8}, {4,8}, {3,8}, {1,8},
                   {2,9}, {4,9}, {3,9}, {1,9},
                   {2,10}, {4,10}, {3,10}, {1,10},
                   {2,11}, {4,11}, {3,11}, {1,11},
                   {2,12}, {4,12}, {3,12}, {1,12},
                   {2,13}, {4,13}, {3,13}, {1,13},
                   {2,1}, {4,1}, {3,1}, {1,1},
                   {2,2}, {4,2}, {3,2}, {1,2},
                   {2,3}, {4,3}, {3,3}, {1,3},
                   {2,4}, {4,4}, {3,4}, {1,4},
                   {2,5}, {4,5}, {3,5}, {1,5},
                   {2,6}, {4,6}, {3,6}, {1,6},
                   {2,7}, {4,7}, {3,7}, {1,7},
                   {2,8}, {4,8}, {3,8}, {1,8},
                   {2,9}, {4,9}, {3,9}, {1,9},
                   {2,10}, {4,10}, {3,10}, {1,10},
                   {2,11}, {4,11}, {3,11}, {1,11},
                   {2,12}, {4,12}, {3,12}, {1,12},
                   {2,13}, {4,13}, {3,13}, {1,13}
                  ]),
    A ++ [?FALSE_OKEY, ?FALSE_OKEY].

pieces_set_1() ->
    set_pieces(set_1()).

pieces_set_2() ->
    set_pieces(set_2()).

to_pieces(Hands) ->
    [#'OkeyPiece'{color = Color, value = Value} || {Color, Value} <- Hands].

are_hands_proper_test() ->
    {A, LL1, L2} = game_okey:generate_hand(),
    Ideal = lists:sort([A] ++ lists:flatten(LL1) ++ L2),
    Set1 = lists:sort(set_1()),
    Set2 = lists:sort(set_2()),
    gas:info(?MODULE,"~nIdeal -- Set1 = ~p~nSet1 -- Ideal = ~p~n", [Ideal -- Set1, Set1 -- Ideal]),
    gas:info(?MODULE,"~nIdeal -- Set2 = ~p~nSet2 -- Ideal = ~p~n", [Ideal -- Set2, Set2 -- Ideal]),
    true = Ideal == Set1,
    true = Ideal == Set2.
