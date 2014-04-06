-module(lucky_sup).
-behaviour(supervisor).
-include_lib("db/include/config.hrl").
-export([start_link/0]).
-export([init/1]).
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,

    OkeyTableParams = [{mult_factor, 1},
                       {slang_allowed, false},
                       {observers_allowed, false},
                       {tournament_type, lucky},
                       {round_timeout, infinity},
%%%                        {round_timeout, 30 * 1000},
                       {set_timeout, infinity},
%%%                        {set_timeout, 10 * 60 *1000},
                       {speed, normal},
                       {game_type, standard},
                       {rounds, undefined},
                       {reveal_confirmation, true},
                       {next_series_confirmation, no},
                       {pause_mode, normal},
                       {social_actions_enabled, true},
                       {gosterge_finish_allowed, undefined}
                      ],
    OkeyGameId = id_generator:get_id(),
    GameName = "I'm filling lucky - " ++ erlang:integer_to_list(OkeyGameId),
    OkeyParams = [{game, game_okey},
                  {game_mode, standard},
                  {game_name, GameName},
                  {mode, normal}, % Common table for several real players
                  {seats, 4},
%%%                  {quota_per_round, Quota},
                  {table_module, game_okey_ng_table_trn},
                  {bot_module, game_okey_bot},
                  {table_params, OkeyTableParams}
                 ],
    OkeySpec = {okey_lucky, {nsg_trn_lucky, start_link, [OkeyGameId, OkeyParams]},
                  Restart, Shutdown, worker, [nsg_trn_lucky]},


    TavlaTableParams = [{mult_factor, 1},
                        {slang_allowed, false},
                        {observers_allowed, false},
                        {tournament_type, lucky},
                        {round_timeout, infinity},
%%%                        {round_timeout, 30 * 1000},
                        {set_timeout, infinity},
%%%                        {set_timeout, 10 * 60 *1000},
                        {speed, normal},
                        {game_mode, standard},
                        {rounds, undefined},
                        {next_series_confirmation, no},
                        {pause_mode, normal},
                        {social_actions_enabled, true}
                       ],

    TavlaGameId = id_generator:get_id(),
    TavlaGameName = "I'm filling lucky - " ++ erlang:integer_to_list(TavlaGameId),
    TavlaParams = [{game, game_tavla},
                   {game_mode, standard},
                   {game_name, TavlaGameName},
                   {mode, normal}, % Common table for several real players
                   {seats, 2},
%%%                  {quota_per_round, Quota},
                   {table_module, game_tavla_ng_table},
                   {bot_module, game_tavla_bot},
                   {table_params, TavlaTableParams}
                  ],
    TavlaSpec = {tavla_lucky, {nsg_trn_lucky, start_link, [TavlaGameId, TavlaParams]},
                  Restart, Shutdown, worker, [nsg_trn_lucky]},

%%     TavlaSpec = {tavla_lucky, {fl_lucky, start_link, [TavlaGameId,[{game_type, game_tavla}, {mode, normal}]]}, 
%%                   Restart, Shutdown, worker, [fl_lucky]},

    {ok, { SupFlags, [OkeySpec, TavlaSpec]} }.

