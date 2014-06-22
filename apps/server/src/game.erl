-module(game).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-compile(export_all).
-include_lib("server/include/requests.hrl").
-include_lib("db/include/journal.hrl").
-include_lib("db/include/table.hrl").
-include_lib("db/include/tournaments.hrl").
-include_lib("db/include/scoring.hrl").
-include_lib("stdlib/include/qlc.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

online() -> [X||X<-qlc:e(gproc:table()),element(1,X)=={p,l,broadcast}].

get_all_games_ids() ->
    [GameId || {_, _, #game_table{id = GameId, game_type = game_okey}} <- qlc:e(gproc:table())].

destroy_game(Pid,Sup) -> game_sup:stop_game(Sup,Pid).

gen_game_id() ->
    PoolNum = wf:config(nsx_idgen,game_pool,5000000) div 1000000,
    PoolNumStr = integer_to_list(PoolNum),
    PoolNum*1000000 + 200 + kvs:next_id(game_table, 1).
     %% 200 is reserved for lucky games and for already created games

create_game(GameId, GameFSM, Params) ->
    {ok, Pid} = create_game_monitor(GameId, GameFSM, Params),
    {ok, GameId, Pid}.

%% rank_table(GameId) -> {ok, {LastTourNum, TourResult}} | {error, Reason}
%% TourResult = {UserId, Pos, Points, Status}
rank_table(GameId) ->
    case get_relay_mod_pid(GameId) of
        {Module, Pid} -> Module:system_request(Pid, last_tour_result);
        undefined -> {error, not_found}
    end.

get_lucky_pid(Sup) ->
    [X]=game:get_lucky_table(Sup),
    X#game_table.game_process.
get_relay_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P} | _] -> gas:info(?MODULE,"GameRelay: ~p",[P]), P end.
get_relay_mod_pid(GameId) -> case get_tables(GameId) of [] -> undefined;
    [#game_table{game_process = P, game_module = M} | _] ->  gas:info(?MODULE,"GameRelay: ~p",[{M,P}]), {M,P} end.
get_relay(GameId) -> gen_server:call(?MODULE, {get_relay, GameId}).
game_requirements(GameAtom) -> GameAtom:get_requirements().
game_requirements(game_tavla,paired) -> paired_tavla:get_requirements();
game_requirements(GameAtom,_) -> GameAtom:get_requirements().
counter(Game) -> PL = supervisor:count_children(case Game of game_okey -> okey_sup; 
                                                            game_tavla -> tavla_sup; _ -> game_sup end),
                 Res = proplists:get_value(active, PL, 0),
                 case Game of
                      game_okey -> Res;
                      game_tavla -> Res;
                      _ -> 0 end.

game_sup_domain(Module, Params) ->
    case Module of
        tavla_paired -> tavla_sup;
        standalone ->
            case proplists:get_value(game, Params) of
                game_okey -> okey_sup;
                game_tavla -> tavla_sup;
                _ -> game_sup
            end;
        elimination ->
            case proplists:get_value(game_type, Params) of
                game_okey -> okey_sup;
                game_tavla -> tavla_sup;
                _ -> game_sup
            end;
        _ -> game_sup
    end.

create_game_monitor(Topic, GameFSM, Params) ->
    Sup = game_sup_domain(GameFSM, Params),
    gas:info(?MODULE,"Create Root Game Process (Game Monitor2): ~p Params: ~p Sup: ~p",[GameFSM, Params,Sup]),
    RelayInit = Sup:start_game(GameFSM,[Topic,Params],Topic),
    gas:info(?MODULE,"RelayInit ~p",[RelayInit]),
    RelayInit.

get_lucky_table(Game) ->
    Lucky = true,
    Check = fun(undefined, _Value) -> true;
               (Param, Value) ->  Param == Value
            end,
    Cursor = fun() ->
                     qlc:cursor(qlc:q([V || {{_,_,_K},_,V=#game_table{game_type=G,
                                                                      feel_lucky = L}}
                                                <- gproc:table(props),
                                            Check(Game, G),
                                            Check(Lucky, L)]))
             end,
    Tables = qlc:next_answers(Cursor(), 1),
    Tables.

get_tournament(TrnId) ->
    Check = fun(undefined, _Value) -> true;
               (Param, Value) ->  Param == Value
            end,
    Cursor = fun() ->
                     qlc:cursor(qlc:q([V || {{_,_,_K},_, V = #game_table{trn_id=TId}} <- gproc:table(props),
                                            Check(TrnId, TId)]))
             end,
    Table = case qlc:next_answers(Cursor(), 1) of
                   [T] -> X = T#game_table.id, X;
                     _ -> []
            end,
%    gas:info(?MODULE,"~w:get_tournament Table = ~p", [?MODULE, Table]),
    Table.


%% stress_test(NumberOfRooms) ->
%%     OkeyPlayers = [begin
%%           {ok,GameId,A} = game:create_table(game_okey,[{table_name,"okey maxim and alice + 2 robots"},
%%                           {speed,normal},
%%                           {rounds,80},
%%                           {sets,1},
%%                           {game_mode,standard},
%%                           {owner,"kate"}],[<<"maxim">>,<<"alice">>,robot,robot]),
%% 
%%             Clients = [ proc_lib:spawn_link(fun() -> 
%%                                  test_okey:init_with_join_game(self(), '127.0.0.1', ?LISTEN_PORT, GameId, Id, 1, normal)
%%                         end) || Id <- [<<"maxim">>,<<"alice">>] ],
%% 
%%                     {ok,GameId,A}
%%                   
%% 
%%                    end ||X<-lists:seq(1,NumberOfRooms)],
%%     [{ok,OP1,_}|_] = OkeyPlayers,
%%     [{ok,OP2,_}|_] = lists:reverse(OkeyPlayers),
%%     gas:info(?MODULE,"Okey bot rooms runned (STRESS): ~p~n",[{OP1,OP2}]).


create_standalone_game(Game, Params, Users) ->
    gas:info(?MODULE,"create_standalone_game/3 Params:~p", [Params]),
    case Game of
        game_okey ->
            #pointing_rule{quota = Quota,
                           kakush_winner = KakushForWinners,
                           kakush_other = KakushForLoser,
                           game_points = WinGamePoints
                          } = proplists:get_value(pointing_rules, Params),
            GameId = proplists:get_value(game_id, Params),
            TableName = proplists:get_value(table_name, Params),
            MulFactor = proplists:get_value(double_points, Params, 1),
            SlangAllowed = proplists:get_value(slang, Params, false),
            ObserversAllowed = proplists:get_value(observers, Params, false),
            Speed = proplists:get_value(speed, Params, normal),
            GameMode = proplists:get_value(game_mode, Params),
            Rounds = case GameMode of
                         countdown -> undefined;
                         _ -> proplists:get_value(rounds, Params, undefined)
                     end,
            GostergeFinishAllowed = proplists:get_value(gosterge_finish, Params, false),
            BotsReplacementMode = case proplists:get_value(robots_replacement_allowed, Params, true) of
                                      true -> enabled;
                                      false -> disabled
                                  end,
            TableParams = [
                           {table_name, TableName},
                           {mult_factor, MulFactor},
                           {slang_allowed, SlangAllowed},
                           {observers_allowed, ObserversAllowed},
                           {tournament_type, standalone},
                           {round_timeout, infinity},
%%                           {round_timeout, 30 * 1000},
                           {set_timeout, infinity},
%%                           {set_timeout, 10 * 60 *1000},
                           {speed, Speed},
                           {game_type, GameMode},
                           {rounds, Rounds},
                           {reveal_confirmation, true},
                           {next_series_confirmation, no_exit},
                           {pause_mode, normal},
                           {social_actions_enabled, true},
                           {gosterge_finish_allowed, GostergeFinishAllowed}
                         ],

            create_game(GameId, standalone,
                         [{game, Game},
                          {game_mode, GameMode},
                          {game_name, TableName},
                          {seats, 4},
                          {registrants, Users},
                          {initial_points, 0},
                          {quota_per_round, Quota},
                          {kakush_for_winners, KakushForWinners},
                          {kakush_for_loser, KakushForLoser},
                          {win_game_points, WinGamePoints},
                          {mul_factor, MulFactor},
                          {table_module, okey_table},
                          {bot_module, okey_bot},
                          {bots_replacement_mode, BotsReplacementMode},
                          {table_params, TableParams},
                          {common_params, Params}
                         ]);
        game_tavla ->
            #pointing_rule{quota = Quota,
                           kakush_winner = KakushForWinners,
                           kakush_other = KakushForLoser,
                           game_points = WinGamePoints
                          } = proplists:get_value(pointing_rules, Params),
            GameId = proplists:get_value(game_id, Params),
            TableName = proplists:get_value(table_name, Params),
            MulFactor = proplists:get_value(double_points, Params, 1),
            SlangAllowed = proplists:get_value(slang, Params, false),
            ObserversAllowed = proplists:get_value(observers, Params, false),
            Speed = proplists:get_value(speed, Params, normal),
            GameMode = proplists:get_value(game_mode, Params),
            Rounds = case GameMode of
                         _ -> proplists:get_value(rounds, Params, undefined)
                     end,
            BotsReplacementMode = case proplists:get_value(robots_replacement_allowed, Params, true) of
                                      true -> enabled;
                                      false -> disabled
                                  end,
            TableParams = [
                           {table_name, TableName},
                           {mult_factor, MulFactor},
                           {slang_allowed, SlangAllowed},
                           {observers_allowed, ObserversAllowed},
                           {tournament_type, standalone},
                           {round_timeout, infinity},
%%                           {round_timeout, 30 * 1000},
                           {set_timeout, infinity},
%%                           {set_timeout, 10 * 60 *1000},
                           {speed, Speed},
                           {game_mode, GameMode},
                           {rounds, Rounds},
                           {next_series_confirmation, no_exit},
                           {pause_mode, normal},
                           {social_actions_enabled, true},
                           {tables_num, 1}
                         ],

            create_game(GameId, standalone,
                         [{game, Game},
                          {game_mode, GameMode},
                          {game_name, TableName},
                          {seats, 2},
                          {registrants, Users},
                          {initial_points, 0},
                          {quota_per_round, Quota},
                          {kakush_for_winners, KakushForWinners},
                          {kakush_for_loser, KakushForLoser},
                          {win_game_points, WinGamePoints},
                          {mul_factor, MulFactor},
                          {table_module, tavla_table},
                          {bot_module, tavla_bot},
                          {bots_replacement_mode, BotsReplacementMode},
                          {table_params, TableParams},
                          {common_params, Params}
                         ])
    end.


create_paired_game(Game, Params, Users) ->
    gas:info(?MODULE,"create_paired_game/3 Params:~p", [Params]),
    case Game of
        game_tavla ->
            #pointing_rule{quota = Quota,
                           kakush_winner = KakushForWinners,
                           kakush_other = KakushForLoser,
                           game_points = WinGamePoints
                          } = proplists:get_value(pointing_rules, Params),
            GameId = proplists:get_value(game_id, Params),
            TableName = proplists:get_value(table_name, Params),
            MulFactor = proplists:get_value(double_points, Params, 1),
            SlangAllowed = proplists:get_value(slang, Params, false),
            ObserversAllowed = proplists:get_value(observers, Params, false),
            Speed = proplists:get_value(speed, Params, normal),
            GameMode = proplists:get_value(game_mode, Params),
            Rounds = case GameMode of
                         _ -> proplists:get_value(rounds, Params, undefined)
                     end,
            BotsReplacementMode = case proplists:get_value(robots_replacement_allowed, Params, true) of
                                      true -> enabled;
                                      false -> disabled
                                  end,
            TablesNum = length(Users) div 2 + length(Users) rem 2,
            TableParams = [
                           {table_name, TableName},
                           {mult_factor, MulFactor},
                           {slang_allowed, SlangAllowed},
                           {observers_allowed, ObserversAllowed},
                           {tournament_type, paired},
                           {round_timeout, infinity},
                           {set_timeout, infinity},
                           {speed, Speed},
                           {game_mode, GameMode},
                           {rounds, Rounds},
                           {next_series_confirmation, no_exit},
                           {pause_mode, disabled},
                           {social_actions_enabled, true},
                           {tables_num, TablesNum}
                         ],

            create_game(GameId, tavla_paired,
                         [{game, Game},
                          {game_mode, GameMode},
                          {game_name, TableName},
                          {tables_num, TablesNum},
                          {registrants, Users},
                          {quota_per_round, Quota},
                          {kakush_for_winners, KakushForWinners},
                          {kakush_for_loser, KakushForLoser},
                          {win_game_points, WinGamePoints},
                          {mul_factor, MulFactor},
                          {table_module, tavla_table},
                          {bot_module, tavla_bot},
                          {bots_replacement_mode, BotsReplacementMode},
                          {table_params, TableParams},
                          {common_params, Params}
                         ])
    end.


create_elimination_trn(GameType, Params, Registrants) ->
    gas:info(?MODULE,"create_elimination_trn/3 Params:~p", [Params]),
    TrnId         = proplists:get_value(trn_id, Params),
    QuotaPerRound = proplists:get_value(quota_per_round, Params),
    PlayersNumber = proplists:get_value(players_number, Params),
    Tours         = proplists:get_value(tours, Params),
    GameMode      = proplists:get_value(game_mode, Params),
    Speed         = proplists:get_value(speed, Params),
    Awards        = proplists:get_value(awards, Params),
    RegistrantsNum = length(Registrants),
    if RegistrantsNum =/= PlayersNumber ->
           gas:error(?MODULE,"create_elimination_trn/3 Error: Wrong number of the registrants: ~p (required: ~p). ",
                  [RegistrantsNum, PlayersNumber]),
           exit(wrong_registrants_number);
       true -> do_nothing
    end,
    {ok, Plan} = matrix:get_plan(GameType, QuotaPerRound, PlayersNumber, Tours),
    case GameType of
        game_okey ->
            Rounds = 10,
            {ok, SetTimeout} = nsm_db:get(config,"games/okey/trn/elim/tour_time_limit/"++integer_to_list(Tours), 35*60*1000),
            TableParams = [
                           {table_name, ""},
                           {tournament_type, elimination},
                           {round_timeout, infinity},
                           {set_timeout, SetTimeout},
                           {speed, Speed},
                           {game_type, GameMode},
                           {rounds, Rounds},
                           {gosterge_finish_allowed, undefined},
                           {reveal_confirmation, true},
                           {next_series_confirmation, no},
                           {pause_mode, disabled},
                           {observers_allowed, false},
                           {slang_allowed, false},
                           {social_actions_enabled, false},
                           {mult_factor, 1}
                         ],
            create_game(TrnId, elimination,
                        [{game_type, GameType},
                         {game_mode, GameMode},
                         {registrants, Registrants},
                         {plan, Plan},
                         {quota_per_round, QuotaPerRound},
                         {rounds_per_tour, Rounds},
                         {tours, Tours},
                         {players_per_table, 4},
                         {speed, Speed},
                         {awards, Awards},
                         {trn_id, TrnId},
                         {table_module, okey_table},
                         {demo_mode, false},
                         {table_params, TableParams}
                        ]);
        game_tavla ->
            Rounds = 3,
            {ok, SetTimeout} = nsm_db:get(config,"games/tavla/trn/elim/tour_time_limit/"++integer_to_list(Tours), 35*60*1000),
            TableParams = [
                           {table_name, ""},
                           {tournament_type, elimination},
                           {round_timeout, infinity},
                           {set_timeout, SetTimeout},
                           {speed, Speed},
                           {game_mode, GameMode},
                           {rounds, Rounds},
                           {next_series_confirmation, no},
                           {pause_mode, disabled},
                           {slang_allowed, false},
                           {observers_allowed, false},
                           {social_actions_enabled, false},
                           {mult_factor, 1},
                           {tables_num, 1}
                         ],

            create_game(TrnId, elimination,
                        [{game_type, GameType},
                         {game_mode, GameMode},
                         {registrants, Registrants},
                         {plan, Plan},
                         {quota_per_round, QuotaPerRound},
                         {rounds_per_tour, Rounds},
                         {tours, Tours},
                         {players_per_table, 2},
                         {speed, Speed},
                         {awards, Awards},
                         {trn_id,TrnId},
                         {table_module, tavla_table},
                         {demo_mode, false},
                         {table_params, TableParams}
                        ])
    end.


start_tournament(TrnId,NumberOfTournaments,NumberOfPlayers,_Quota,_Tours,_Speed,GiftIds) ->
    gas:info(?MODULE,"START TOURNAMENT: ~p",[{TrnId,NumberOfTournaments,NumberOfPlayers,_Quota,_Tours,_Speed,GiftIds}]),
    {ok,Tournament} = nsm_db:get(tournament,TrnId),
    RealPlayersUnsorted = nsm_tournaments:joined_users(TrnId),

 if NumberOfPlayers - length(RealPlayersUnsorted) > 300 ->
           nsm_db:put(Tournament#tournament{status=canceled}),
           wf:send([tournament, TrnId, cancel], {TrnId}),
           error;
       true ->

    #tournament{quota = QuotaPerRound,
                tours = Tours,
                game_type = GameType,
                game_mode = GameMode,
                speed = Speed} = Tournament,

    RealPlayersPR = lists:keysort(#play_record.other, RealPlayersUnsorted),
    gas:info(?MODULE,"Head: ~p",[hd(RealPlayersPR)]),
    RealPlayers = [list_to_binary(Who)||#play_record{who=Who}<-RealPlayersPR, Who /= undefined],

%%     Registrants = case NumberOfPlayers > length(RealPlayers) of
%%                        true -> nsm_db:put(Tournament#tournament{status=canceled}), RealPlayers;
%%                        false -> [lists:nth(N,RealPlayers)||N<-lists:seq(1,NumberOfPlayers)] end,

    RealPlayersNumber = length(RealPlayers),

    Registrants = if NumberOfPlayers == RealPlayersNumber -> RealPlayers;
                     NumberOfPlayers > RealPlayersNumber ->
                         RealPlayers ++ [list_to_binary(anonymous:ima_gio(N)) ||
                                            N <- lists:seq(1, NumberOfPlayers-RealPlayersNumber)];
                     true -> lists:sublist(RealPlayers, NumberOfPlayers)
                  end,

    gas:info(?MODULE,"Registrants: ~p",[Registrants]),
    OkeyTournaments =
        [begin
             Params = [{trn_id, TrnId},
                       {quota_per_round, QuotaPerRound},
                       {players_number, NumberOfPlayers},
                       {tours, Tours},
                       {game_mode, GameMode},
                       {speed, Speed},
                       {awards, GiftIds}],
             {ok,GameId,A} = create_elimination_trn(GameType, Params, Registrants),
             nsm_db:put(Tournament#tournament{status=activated,start_time=time()}),
             wf:send([tournament, TrnId, activate], {TrnId}),
             {ok,GameId,A}
         end || _ <-lists:seq(1,NumberOfTournaments)],
    [{ok,OP1,_}|_] = OkeyTournaments,
    [{ok,OP2,_}|_] = lists:reverse(OkeyTournaments),
    gas:info(?MODULE,"Okey tournaments runned: ~p~n",[{OP1,OP2}]),
    OP1

   end.

get_tables(Id) ->
   qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{id = _Id}} <- gproc:table(props), Id == _Id ])).

qlc_id(Id) ->
    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{gameid = _GameId, id = _Id, 
                            owner = _Owner, creator = _Creator}} <- 
             gproc:table(props), Id == _Id])).

qlc_id_creator(Id,Creator,Owner) ->
    qlc:e(qlc:q([Val || {{_,_,_Key},_,Val=#game_table{gameid = _GameId, id = _Id, 
                            owner = _Owner, creator = _Creator}} <- 
             gproc:table(props), Id == _Id, Creator == _Creator, Owner ==_Owner])).

campaigns(reveal) -> campaigns(reveal,{2014,4,27},{2014,7,30});
campaigns(series) -> campaigns(series,{2014,4,27},{2014,7,30}).

campaigns(series,From,To) ->
    {atomic,Res}=mnesia:transaction(fun() -> mnesia:select(series_event,
        ets:fun2ms(fun (#series_event{date=T}=S) when T > From, T < To -> S end)) end),
    lists:keysort(2,lists:foldr(fun series_aggregate/2,[],Res));

campaigns(reveal,From,To) ->
    {atomic,Res}=mnesia:transaction(fun() -> mnesia:select(reveal_event,
        ets:fun2ms(fun (#reveal_event{date=T}=S) when T > From, T < To -> S end)) end),
    lists:keysort(2,lists:foldr(fun reveal_aggregate/2,[],Res)).

series_aggregate(#series_event{score = Count, user = Item}, Acc) ->
    case lists:keyfind(Item,1,Acc) of
        {Item,Sum} -> lists:keyreplace(Item,1,Acc,{Item,Count+Sum});
        false -> [{Item,Count}|Acc] end.

reveal_aggregate(#reveal_event{score = Count, user = Item}, Acc) ->
    case lists:keyfind(Item,1,Acc) of
        {Item,Sum} -> lists:keyreplace(Item,1,Acc,{Item,Count+Sum});
        false -> [{Item,Count}|Acc] end.

get_player_info(_,User) ->
    Okey = okey_scoring,
    Scoring = [ begin 
        case kvs:get(series_log,{M,S,R,User}) of
       {ok,#series_log{type=M,speed=S,rounds=R,stats=Res}} ->
            Win = case lists:keyfind(winner,1,Res) of {_,Num1} -> Num1; _ -> 0 end,
            Los = case lists:keyfind(looser,1,Res) of {_,Num2} -> Num2; _ -> 0 end,
            [{lists:concat([wf:to_list(M)," ",wf:to_list(S)," ",wf:to_list(R)]),
              lists:concat([Win,"/",Los])}];
       _ -> [] end end || M <- Okey:modes(), S <- Okey:speeds(), R <- Okey:rounds() ],
    Games=lists:flatten(Scoring),
    Reveals  = case kvs:get(reveal_log,User)   of {ok,R} -> R; _ -> #reveal_log{} end,
    Protocol = case kvs:get(protocol_log,User) of {ok,P} -> P; _ -> #protocol_log{} end,
    #stats_event{
        player_id=User,
        games=Games,
        reveals=Reveals#reveal_log.stats,
        protocol=Protocol#protocol_log.stats,
        score=case Reveals#reveal_log.score of undefined -> <<"0">>; E -> integer_to_binary(E) end}.

plist_setkey(Name,Pos,List,New) ->
    case lists:keyfind(Name,Pos,List) of
        false -> [New|List];
        Element -> lists:keyreplace(Name,Pos,List,New) end.
