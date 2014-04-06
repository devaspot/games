-module(game_stats).
-behaviour(gen_server).

-export([start_link/0, add_game/1, get_skill/1, get_game_points/2, get_player_stats/1,
         init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         assign_points/2, is_feel_lucky/1, game_info_to_ti/1, charge_quota/1]).

-include_lib("server/include/basic_types.hrl").
-include_lib("server/include/game_okey.hrl").
-include_lib("server/include/game_tavla.hrl").
-include_lib("server/include/log.hrl").
-include_lib("server/include/games.hrl").
-include_lib("db/include/accounts.hrl").
-include_lib("db/include/scoring.hrl").


-record(raw_result,
        {player_id :: binary(),
         winner    :: boolean(),
         score     :: integer(),
         pos       :: integer()
        }).

-record(result,
        {player_id :: string(),
         robot     :: boolean(),
         winner    :: boolean(),
         score     :: integer(),
         pos       :: integer(),
         kakush_points :: integer(),
         game_points :: integer()
        }).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
add_game(Game) -> gen_server:cast(?MODULE, {add_game, Game}).

get_skill(UserId) when is_binary(UserId) -> get_skill(binary_to_list(UserId));
get_skill(UserId) -> {ok, crypto:rand_uniform(1,1000)}.

get_game_points(GameType, UserId) when is_binary(UserId) -> get_game_points(GameType, binary_to_list(UserId));
get_game_points(GameType, UserId) -> {ok, [{game_points,crypto:rand_uniform(1,1000)},
                                           {finished_with_okey,crypto:rand_uniform(1,1000)},
                                           {finished_with_8_tashes,crypto:rand_uniform(1,1000)}]}.

get_player_stats(UserId) when is_binary(UserId) -> get_player_stats(binary_to_list(UserId));
get_player_stats(UserId) -> {ok, [{total_games,crypto:rand_uniform(1,10)},
                                  {total_wins,crypto:rand_uniform(1,10)},
                                  {total_loses,crypto:rand_uniform(1,10)},
                                  {overal_success_ratio,crypto:rand_uniform(1,100)},
                                  {average_play_time,crypto:rand_uniform(1000,5000)}]}.

init([]) -> {ok, no_state}.

handle_call(Request, From, State) ->
    error_logger:error_msg("unknown call ~p ~p ~n", [Request, From]),
    {noreply, State}.

handle_cast({add_game, Game}, State) -> {noreply, State};
handle_cast(Msg, State) -> error_logger:error_msg("unknown cast ~p ~n", [Msg]), {noreply, State}.
handle_info(Info, State) -> error_logger:error_msg("unknown info ~p~n", [Info]), {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

is_feel_lucky(GameInfo) ->
    proplists:get_value(lucky, GameInfo,false).

game_info_to_ti(GameInfo) ->
    #ti_game_event{game_name = okey,
                   game_mode = proplists:get_value(mode, GameInfo),
                   id = proplists:get_value(id, GameInfo),
                   double_points = proplists:get_value(double_points, GameInfo)
                  }.


charge_quota(GameInfo) ->
    PR0       = proplists:get_value(pointing_rules, GameInfo),
    PRLucky   = proplists:get_value(pointing_rules_lucky, GameInfo),
    Players   = proplists:get_value(initial_players, GameInfo),
    Double    = proplists:get_value(double_points, GameInfo),
    TI = game_info_to_ti(GameInfo),
    PR = pointing_rules:double_points(PR0, Double),
    [begin
         UId = user_id_to_string(U#'PlayerInfo'.id),
         Amount = case is_feel_lucky(GameInfo) of
                      true -> PRLucky#pointing_rule.quota;
                      _ -> PR#pointing_rule.quota
                  end,
        ok = nsm_accounts:transaction(UId, ?CURRENCY_QUOTA, -Amount, TI#ti_game_event{type = game_start})
     end || U  <- Players].

assign_points(#'TavlaGameResults'{players = Results}, GameInfo) ->
    ConvertedResults = [ #raw_result{winner = Winner == <<"true">>, player_id = PlayerId, score = Score,
                                     pos = if Winner == <<"true">> -> 1; true -> 2 end}
                        || #'TavlaPlayerScore'{winner = Winner, player_id = PlayerId, score = Score} <- Results],
    assign_points(ConvertedResults, GameInfo);

assign_points(#'OkeyGameResults'{series_results = Results}, GameInfo) ->
    ConvertedResults = [ #raw_result{winner = Winner == <<"true">>, player_id = PlayerId, score = Score, pos = Pos}
                        || #'OkeySeriesResult'{winner = Winner, player_id = PlayerId, score = Score, place = Pos} <- Results],
    assign_points(ConvertedResults, GameInfo);

assign_points(RawResults, GameInfo) ->
    GameId   = proplists:get_value(id, GameInfo),
    PR0      = proplists:get_value(pointing_rules, GameInfo),
    PRLucky  = proplists:get_value(pointing_rules_lucky, GameInfo),
    Players  = proplists:get_value(initial_players, GameInfo),
    Double   = proplists:get_value(double_points, GameInfo),
    TI = game_info_to_ti(GameInfo),

    PR1 = pointing_rules:double_points(PR0, Double),

    PlayersIds = [if Robot -> user_id_to_string(UserId);
                     true -> "(robot)"
                  end || #'PlayerInfo'{id = UserId, robot = Robot} <- Players],

    #pointing_rule{
                   kakush_winner = KakushWinner,
                   kakush_other = KakushOther,
                   game_points = WinGamePoints
                  } = case is_feel_lucky(GameInfo) of true -> PRLucky; false -> PR1 end,

    Bots    = [UserId || #raw_result{player_id = UserId} <- RawResults, is_bot(UserId, Players)],
    Paids   = [UserId || #raw_result{player_id = UserId} <- RawResults, is_paid(UserId)],
    Winners = [UserId || #raw_result{player_id = UserId, winner = true} <- RawResults],
    TotalNum = length(RawResults),
    PaidsNum = length(Paids),
    WinnersNum = length(Winners),
    KakushPerWinner = round((KakushWinner * PaidsNum div TotalNum) / WinnersNum),
    KakushPerLoser = KakushOther * PaidsNum div TotalNum,
    ?INFO("GAME_STATS <~p> KakushWinner: ~p KakushOther: ~p", [GameId, KakushWinner, KakushOther]),
    ?INFO("GAME_STATS <~p> KakushPerWinner: ~p KakushPerLoser: ~p", [GameId, KakushPerWinner, KakushPerLoser]),
    Results = [begin
                   Robot = lists:member(UserId, Bots),
                   Paid = lists:member(UserId, Paids),
                   {KakushPoints, GamePoints} = calc_points(KakushPerWinner, KakushPerLoser,
                                                            WinGamePoints, Paid, Robot, Winner),
                   #result{player_id = user_id_to_string(UserId), robot = Robot, winner = Winner,
                           pos = Pos, kakush_points = KakushPoints, game_points = GamePoints}
               end || #raw_result{player_id = UserId, winner = Winner, pos = Pos} <- RawResults],
    ?INFO("GAME_STATS <~p> Results: ~p", [GameId, Results]),
    [begin
         if not Robot ->
                SR = #scoring_record{
                                     game_id = proplists:get_value(id, GameInfo),
                                     who = UserId,
                                     all_players = PlayersIds,
                                     game_type = PR0#pointing_rule.game_type,
                                     game_kind = PR0#pointing_rule.game,
%                                     condition, % where'd I get that?
                                     score_points = GamePoints,
                                     score_kakaush = KakushPoints,
%                                     custom,    % no idea what to put here
                                     timestamp = erlang:now()
                                    },
                Route = [feed, user, UserId, scores, 0, add],  % maybe it would require separate worker for this
                nsx_msg:notify(Route, [SR]),
                {Wins, Loses} = if Winner-> {1, 0};
                                   true -> {0, 1}
                                end,
                % haven't found a way to properly get average time
                nsx_msg:notify([personal_score, user, UserId, add],
                               {_Games = 1, Wins, Loses, _Disconnects = 0, GamePoints, 0});
            true -> do_nothing  % no statistics for robots
         end,
         if not Robot ->
                if KakushPoints /= 0 ->
                       ok = nsm_accounts:transaction(UserId, ?CURRENCY_KAKUSH, KakushPoints, TI#ti_game_event{type = game_end});
                   true -> ok
                end,
                if GamePoints /= 0 ->
                        ok = nsm_accounts:transaction(UserId, ?CURRENCY_GAME_POINTS, GamePoints, TI#ti_game_event{type = game_end});
                   true -> ok
                end;
            true -> do_nothing %% no points for robots
         end
     end || #result{player_id = UserId, robot = Robot, winner = Winner,
                    kakush_points = KakushPoints, game_points = GamePoints}  <- Results],

    GameName = proplists:get_value(name, GameInfo, ""),
    GameType = proplists:get_value(game_type, GameInfo),
    GameEndRes = [{if Robot -> "robot"; true -> UserId end, Robot, Pos, KPoints, GPoints}
                  || #result{player_id = UserId, robot = Robot, pos = Pos,
                             kakush_points = KPoints, game_points = GPoints} <- Results],
    ?INFO("GAME_STATS <~p> Notificaton: ~p", [GameId, {{GameName, GameType}, GameEndRes}]),
    nsx_msg:notify(["system", "game_ends_note"], {{GameName, GameType}, GameEndRes}).

is_bot(UserId, Players) ->
    case lists:keyfind(UserId, #'PlayerInfo'.id, Players) of
        #'PlayerInfo'{robot = Robot} -> Robot;
        _ -> true % If UserId is not found then the player is a replaced bot. 
    end.

is_paid(UserId) -> nsm_accounts:user_paid(UserId).

user_id_to_string(UserId) -> binary_to_list(UserId).

calc_points(KakushPerWinner, KakushPerLoser, WinGamePoints, Paid, Robot, Winner) ->
    if Robot -> {0, 0};
       not Paid andalso Winner -> {0, WinGamePoints};
       not Paid -> {0, 0};
       Paid andalso Winner -> {KakushPerWinner, WinGamePoints};
       Paid -> {KakushPerLoser, 0}
    end.
