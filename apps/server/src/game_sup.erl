-module(game_sup).
-behaviour(supervisor).
-export([start_link/0, stop/0]).
-include_lib("server/include/conf.hrl").
-export([init/1, start/0, start_game/3, stop_game/2]).
-define(SERVER, ?MODULE).

start() -> supervisor:start({local, ?SERVER}, ?MODULE, []).
start_link() -> supervisor:start_link({local, ?SERVER}, ?MODULE, []).
stop() -> exit(?SERVER, shutdown).
start_game(Sup,Mod,Par) -> supervisor:start_child(Sup,[Mod,Par]).
stop_game(Sup,Pid) -> supervisor:terminate_child(Sup,Pid).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 100,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    IdGen = {id_generator, {id_generator, start_link, []},  Restart, Shutdown, worker, [id_generator]},
    AuthServer = {auth_server, {auth_server, start_link, []}, Restart, Shutdown, worker, [auth_server]},
    GameObserver = {game_observer, {game_observer, start_link, []}, Restart, Shutdown, worker, [game_observer]},
    GameStats = {game_stats, {game_stats, start_link, []}, Restart, Shutdown, worker, [game_stats]},
    GameManager = {game_manager, {game_manager, start_link, []}, Restart, Shutdown, worker, [game_manager]},
    TavlaSup = {tavla_sup, {tavla_sup, start_link, []}, Restart, Shutdown, supervisor, [tavla_sup]},
    OkeySup = {okey_sup, {okey_sup, start_link, []}, Restart, Shutdown, supervisor, [okey_sup]},
    LuckySup = {lucky_sup, {lucky_sup, start_link, []}, Restart, Shutdown, supervisor, [lucky_sup]},
    {ok, {SupFlags, [IdGen,GameManager,LuckySup,AuthServer,GameObserver,GameStats,TavlaSup,OkeySup]}}.

