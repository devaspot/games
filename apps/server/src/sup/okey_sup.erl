-module(okey_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-export([init/1, start/0, start_game/3]).
-define(SERVER, ?MODULE).

-define(CROWD_STANDALONE_3PL_NUM, 15).

start() -> supervisor:start({local, ?SERVER}, ?MODULE, []).
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
stop() -> exit(?SERVER, shutdown).
start_game(Mod,Par,GameId) -> 
    gas:info(?MODULE,"OKEY SUP START CHILD"),
    Restart = transient,
    Shutdown = 200,
    ChildSpec = {GameId, {Mod, start_link, Par}, Restart, Shutdown, worker, [Mod]},
    supervisor:start_child(?MODULE,ChildSpec).

init([]) ->
    gas:info(?MODULE,"OKEY SUP STARTED"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Specs = okey_standalone_specs(?CROWD_STANDALONE_3PL_NUM, 3),
    {ok, {SupFlags, Specs}}.


okey_standalone_specs(GamesNum, VirtUsersPerTable) ->
    VirtualUsers = anonymous:virtual_users(),
    if length(VirtualUsers) < VirtUsersPerTable ->
           [];
       true ->
           F = fun(_) ->
                       GameId = game:gen_game_id(),
                       GameName = "Okey/Crowd game - " ++ erlang:integer_to_list(GameId),
                       Users = anonymous:random_users(VirtUsersPerTable, VirtualUsers),
%%%                      Users = [robot, robot, robot],
                       TableParams = [
                                      {table_name, ""},
                                      {mult_factor, 1},
                                      {slang_allowed, false},
                                      {observers_allowed, false},
                                      {tournament_type, standalone},
                                      {round_timeout, infinity},
                                      {set_timeout, infinity},
                                      {speed, fast},
                                      {game_type, standard},
                                      {rounds, lists:nth(crypto:rand_uniform(1,4),okey_scoring:rounds())},
                                      {reveal_confirmation, true},
                                      {next_series_confirmation, no_exit},
                                      {pause_mode, normal},
                                      {social_actions_enabled, true},
                                      {gosterge_finish_allowed, undefined}
                                     ],
                       CommonParams = [{speed, fast},
                                       {rounds, lists:nth(crypto:rand_uniform(1,4),okey_scoring:rounds())},
                                       {double_points, 1},
                                       {game_mode,standard},
                                       {slang, false},
                                       {observers, false},
                                       {owner,"maxim"}
                                      ],
                       Params = [{game, game_okey},
                                 {game_mode, standard},
                                 {game_name, GameName},
                                 {seats, 4},
                                 {registrants, Users},
                                 {initial_points, 0},
                                 {quota_per_round, 1},
                                 {kakush_for_winners, 1},
                                 {kakush_for_loser, 1},
                                 {win_game_points, 1},
                                 {mul_factor, 1},
                                 {table_module, okey_table},
                                 {bot_module, okey_bot},
                                 {bots_replacement_mode, enabled},
                                 {table_params, TableParams},
                                 {common_params, CommonParams} %% This data will used for the gproc register
                                ],
                       {GameId,
                        {standalone, start_link, [GameId, Params]},
                        _Restart = permanent, _Shutdown = 2000, worker, [standalone]}
               end,
           lists:map(F, lists:seq(1, GamesNum))
    end.

