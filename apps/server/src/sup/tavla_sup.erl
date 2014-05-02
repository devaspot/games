-module(tavla_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-export([init/1, start/0, start_game/3]).
-define(SERVER, ?MODULE).

-define(CROWD_STANDALONE_1PL_NUM, 15).

start() -> supervisor:start({local, ?SERVER}, ?MODULE, []).
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
stop() -> exit(?SERVER, shutdown).
start_game(Mod,Par,GameId) -> 
    gas:info(?MODULE,"TAVLA SUP STAR CHILD"),
%%    Restart = transient,
    Restart = temporary,
    Shutdown = 200,
    ChildSpec = {GameId, {Mod, start_link, Par}, Restart, Shutdown, worker, [Mod]},
    supervisor:start_child(?MODULE,ChildSpec).

init([]) ->
    gas:info(?MODULE,"TAVLA SUP STARTED"),
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Specs = tavla_standalone_specs(?CROWD_STANDALONE_1PL_NUM, 1),
    {ok, {SupFlags, Specs}}.

tavla_standalone_specs(GamesNum, VirtUsersPerTable) ->
    VirtualUsers = anonymous:virtual_users(),
    if length(VirtualUsers) < VirtUsersPerTable ->
           [];
       true ->
           F = fun(_) ->
                       GameId = game:gen_game_id(),
                       GameName = "Tavla/Crowd game: " ++ erlang:integer_to_list(GameId),
                       Users = anonymous:random_users(VirtUsersPerTable, VirtualUsers),
%%%                    Users = [robot],
                       TableParams = [
                                      {table_name, ""},
                                      {mult_factor, 1},
                                      {slang_allowed, false},
                                      {observers_allowed, false},
                                      {tournament_type, standalone},
                                      {round_timeout, infinity},
                                      {set_timeout, infinity},
                                      {speed, fast},
                                      {game_mode, standard},
                                      {rounds, 3},
                                      {next_series_confirmation, no_exit},
                                      {pause_mode, normal},
                                      {social_actions_enabled, true},
                                      {tables_num, 1}
                                     ],
                       CommonParams = [{speed, fast},
                                       {rounds, 3},
                                       {double_points, 1},
                                       {game_mode,standard},
                                       {speed, normal},
                                       {slang, false},
                                       {observers, false},
                                       {owner,"maxim"}
                                      ],
                       Params = [{game, game_tavla},
                                 {game_mode, standard},
                                 {game_name, GameName},
                                 {seats, 2},
                                 {registrants, Users},
                                 {initial_points, 0},
                                 {quota_per_round, 1},
                                 {kakush_for_winners, 1},
                                 {kakush_for_loser, 1},
                                 {win_game_points, 1},
                                 {mul_factor, 1},
                                 {table_module, tavla_table},
                                 {bot_module, tavla_bot},
                                 {bots_replacement_mode, enabled},
                                 {table_params, TableParams},
                                 {common_params, CommonParams}
                                ],
                       {GameId,
                        {standalone, start_link, [GameId, Params]},
                        _Restart = permanent, _Shutdown = 2000, worker, [standalone]}
               end,
           lists:map(F, lists:seq(1, GamesNum))
    end.

