%% Author: serge
%% Created: Oct 23, 2012
%% Description:
-module(okey_scoring).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-export([init/3]).

-type normal_tash() :: {integer(), integer()}.
-type tash() :: false_okey | normal_tash().

-define(MODE_STANDARD, standard).
-define(MODE_EVENODD, evenodd).
-define(MODE_COLOR, color).
-define(MODE_COUNTDOWN, countdown).

-define(COUNTDOWN_MAX_POINTS, 10).

-define(ACH_CHANAK_WINNER, 0). %% Not defined in the points matrix

-define(ACH_GOSTERGE_SHOWN, 1).
-define(ACH_WIN_REVEAL, 2).
-define(ACH_WIN_REVEAL_WITH_OKEY, 3).
-define(ACH_WIN_REVEAL_WITH_PAIRS, 4).
-define(ACH_WIN_REVEAL_WITH_OKEY_PAIRS, 5).
-define(ACH_8_TASHES, 6).
-define(ACH_WIN_REVEAL_WITH_COLOR, 7).
-define(ACH_WIN_REVEAL_WITH_COLOR_OKEY, 8).
-define(ACH_WIN_REVEAL_WITH_COLOR_PAIRS, 9).
-define(ACH_WIN_REVEAL_WITH_COLOR_OKEY_PAIRS, 10).
-define(ACH_FAIL_REVEAL, 11).
-define(ACH_CAUGHT_BLUFF, 12).
-define(ACH_EMPTY_BOX, 13).
-define(ACH_REJECT_GOOD_HAND, 14).
-define(ACH_GOSTERGE_WINNER, 15).


-record(state,
        {mode             :: standard | evenodd | color | countdown,
         seats_num        :: integer(),
         rounds_num       :: undefined | pos_integer(),
         last_round_num   :: integer(),
         round_score      :: list(),    %% [{SeatNum, DeltaPoints}]
         round_achs       :: list(),    %% [{SeatNum, [{AchId, Points}]}]
         total_score      :: list(),    %% [{SeatNum, Points}]
         finish_info      :: term(),    %% FinishInfo
         chanak           :: integer()  %% Defined only for evenodd and color mode
        }).

modes() -> [standard,evenodd,color,countdown].
rounds() -> [1,2,3]. %10,20,40,80,undefined].
speeds() -> [slow,normal,fast,undefined].

%%
%% API Functions
%%

%% @spec init(Mode, SeatsInfo, RoundsNum) -> ScoringState
%% @doc Initialises scoring state.
%% @end
%% Types:
%%     Mode = standard | evenodd | color | countdown
%%     SeatsInfo = [{SeatNum, Points}]
%%       SeatNum = integer()
%%       Points = integer()
%%     RoundsNum = undefined | pos_integer()

init(Mode, SeatsInfo, RoundsNum) ->
    gas:info(?MODULE,"OKEY_NG_SCORING init Mode: ~p SeatsInfo = ~p RoundsNum = ~p", [Mode, SeatsInfo, RoundsNum]),
    true = lists:member(Mode, [?MODE_STANDARD, ?MODE_EVENODD, ?MODE_COLOR, ?MODE_COUNTDOWN]),
    true = if Mode == ?MODE_COUNTDOWN -> RoundsNum == undefined;
              true -> is_integer(RoundsNum) orelse RoundsNum == undefined end,
    SeatsNum = length(SeatsInfo),
    true = lists:seq(1, SeatsNum) == lists:sort([SeatNum || {SeatNum, _} <- SeatsInfo]),
    #state{mode = Mode,
           seats_num = SeatsNum,
           rounds_num = RoundsNum,
           last_round_num = 0,
           chanak = 0,
           round_score = undefined,
           finish_info = undefined,
           round_achs = undefined,
           total_score = SeatsInfo
          }.


%% @spec last_round_result(ScoringState) -> {FinishInfo, RoundScore, AchsPoints, TotalScore} |
%%                                          no_rounds_played
%% @end
%% Types:
%%     FinishInfo =  tashes_out | timeout | set_timeout |
%%                   {win_reveal, Revealer, WrongRejects, RevealWithColor, RevealWithOkey, RevealWithPairs} |
%%                   {fail_reveal, Revealer} |
%%                   {gosterge_finish, Winner}
%%     RoundScore = [{SeatNum, DeltaPoints}]
%%     AchsPoints = [{SeatNum, [{AchId, Points}]}]
%%     TotalScore = [{SeatNum, Points}]

last_round_result(#state{last_round_num = 0}) -> no_rounds_played;

last_round_result(#state{round_score = RoundScore,
                         round_achs = RoundAchs,
                         total_score = TotalScore,
                         finish_info = FinishInfo
                        }) ->
    {FinishInfo, RoundScore, RoundAchs, TotalScore}.

%% @spec chanak(ScoringState) -> Chanak
%% @end
%% Types: Chanak = integer()

chanak(#state{chanak = Chanak}) ->
    Chanak.

%% @spec round_finished(ScoringState, FinishReason, Hands, Gosterge, WhoHasGosterge, Has8Tashes) ->
%%                                    {NewScoringState, GameIsOver}
%% @end
%% Types:
%%     FinishReason = tashes_out |
%%                    {reveal, SeatNum, Tashes, Discarded, ConfirmationList} |
%%                    {gosterge_finish, SeatNum}
%%       Tashes = [Row1, Row2]
%%         Row1 = Row2 = [null | tash()]
%%       Discaded = tash()
%%       ConfirmationList = [SeatNum, boolean()]
%%     Hands = [{SeatNum, Hand}]
%%       Hand = [tash()]
%%     Gosterge = normal_tash()
%%     WhoHasGosterge = undefined | SeatNum
%%     Has8Tahses = [SeatNum]
%%     GameisOver = boolean() 

round_finished(#state{mode = GameMode, seats_num = SeatsNum,
                      last_round_num = LastRoundNum,
                      total_score = TotalScore, chanak = Chanak} = State,
               FinishReason, Hands, Gosterge, WhoHasGosterge, Has8Tashes) ->
    ScoringMode = get_scoring_mode(GameMode, Gosterge),
    PointingRules = get_pointing_rules(ScoringMode),
    ChanakRefillingPoints = get_chanak_refilling_points(ScoringMode),
    Seats = lists:seq(1, SeatsNum),
    FinishInfo = finish_info(GameMode, FinishReason, Gosterge),
    PlayersAchs = players_achivements(GameMode, Seats, Hands, WhoHasGosterge, Has8Tashes, FinishInfo),
    {ChanakWinPoints, NewChanak} = calc_chanak(GameMode, WhoHasGosterge, PlayersAchs,
                                               ChanakRefillingPoints, PointingRules, Chanak),
    PlayersAchsRecs = [{SeatNum, get_achivements_points(PointingRules, Achivements)}
                       || {SeatNum, Achivements} <- PlayersAchs],
    RoundAchs = merge_points(PlayersAchsRecs, ChanakWinPoints),
    RoundScore = [{SeatNum, sum_achivements_points(AchPoints)}
                   || {SeatNum, AchPoints} <- RoundAchs],
    RoundNum = LastRoundNum + 1,
    NewTotalScore = add_delta(TotalScore, RoundScore),
    NewState = State#state{last_round_num = RoundNum,
                           round_score = RoundScore,
                           total_score = NewTotalScore,
                           round_achs = RoundAchs,
                           finish_info = FinishInfo,
                           chanak = NewChanak},
    {NewState, detect_game_finish(NewState)}.

%%
%% Local Functions
%%

merge_points(PlayersAchsRecs, ChanakWinPoints) ->
    F = fun({SeatNum, ChanakPoints}, Acc) ->
                ChanakRec = {?ACH_CHANAK_WINNER, ChanakPoints},
                case lists:keyfind(SeatNum, 1, Acc) of
                    {_, AchsRecs} -> lists:keyreplace(SeatNum, 1, Acc,
                                                      {SeatNum, [ChanakRec | AchsRecs]});
                    false ->
                        [{SeatNum, [ChanakRec]}]
                end
        end,
    lists:foldl(F, PlayersAchsRecs, ChanakWinPoints).

%% calc_chanak(GameMode, WhoHasGosterge, PlayersAchs, Chanak) -> {ChanakWinPoints, NewChanak}
calc_chanak(GameMode, WhoHasGosterge, PlayersAchs, RefillingPoints, PointingRules, Chanak) ->
    if GameMode == ?MODE_EVENODD;
       GameMode == ?MODE_COLOR ->
           GostergePoints = if WhoHasGosterge == undefined ->
                                   lists:nth(?ACH_GOSTERGE_SHOWN, PointingRules);
                               true -> 0
                            end,
           case find_chanak_winners(PlayersAchs) of
               [] ->
                   {[], Chanak + GostergePoints};
               ChanakWinners ->
                   WinnersNum = length(ChanakWinners),
                   PerWinnerPoints = Chanak div WinnersNum,
                   RestPoints = Chanak rem WinnersNum,
                   ChanakWinPoints = [{SeatNum, PerWinnerPoints} || SeatNum <- ChanakWinners],
                   {ChanakWinPoints, RestPoints + GostergePoints + RefillingPoints}
           end;
       true -> {[], 0}
    end.


find_chanak_winners(PlayersAchs) ->
    [SeatNum || {SeatNum, Achs} <- PlayersAchs, is_chanak_winner(Achs)].


is_chanak_winner(Achs) ->
    F = fun(Ach) -> lists:member(Ach, chanak_win_achievements())
        end,
    lists:any(F, Achs).


detect_game_finish(#state{mode = GameMode, last_round_num = RoundNum, finish_info = FinishInfo,
                          rounds_num = MaxRoundNum, total_score = TotalScore}) ->
    if GameMode == ?MODE_COUNTDOWN ->
           lists:any(fun({_, Points}) -> Points >= ?COUNTDOWN_MAX_POINTS end, TotalScore);
       FinishInfo == set_timeout ->
           true;
       true ->
           RoundNum == MaxRoundNum
    end.


players_achivements(Mode, Seats, Hands, WhoHasGosterge, Has8Tashes, FinishInfo) ->
    case FinishInfo of
        tashes_out ->
            [begin
                 Achivements = player_achivements_no_winner(Mode, SeatNum, WhoHasGosterge, Has8Tashes),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats];
        timeout ->
            [begin
                 Achivements = player_achivements_no_winner(Mode, SeatNum, WhoHasGosterge, Has8Tashes),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats];
        set_timeout ->
            [begin
                 Achivements = player_achivements_no_winner(Mode, SeatNum, WhoHasGosterge, Has8Tashes),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats];
        {win_reveal, Revealer, WrongRejects, RevealWithColor, RevealWithOkey, RevealWithPairs} ->
            [begin
                 {_, _Hand} = lists:keyfind(SeatNum, 1, Hands),
                 Achivements = player_achivements_win_reveal(Mode, SeatNum, WhoHasGosterge, Has8Tashes,
                                                             Revealer, WrongRejects, RevealWithOkey,
                                                             RevealWithPairs, RevealWithColor),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats];
        {fail_reveal, Revealer} ->
            [begin
                 {_, _Hand} = lists:keyfind(SeatNum, 1, Hands),
                 Achivements = player_achivements_fail_reveal(Mode, SeatNum, WhoHasGosterge,
                                                              Has8Tashes, Revealer),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats];
        {gosterge_finish, _Winner} ->
            [begin
                 {_, _Hand} = lists:keyfind(SeatNum, 1, Hands),
                 Achivements = player_achivements_gosterge_finish(Mode, SeatNum, WhoHasGosterge, Has8Tashes),
                 {SeatNum, Achivements}
             end || SeatNum <- Seats]
    end.

%% TODO: make Finish Info more Universal
        % FinishInfo :: {Reason, Revealer, RevealerWon, WrongRejects, RevealInfo}
        % RevealInfo :: {ColorReveal, OkeyReveal, PairsReveal}

%% finish_info(GameMode, FinishReason, Gosterge) -> 
%%      tashes_out |
%%      timeout |
%%      set_timeout |
%%      {win_reveal, Revealer, WrongRejects, RevealWithColor, RevealWithOkey, RevealWithPairs} |
%%      {fail_reveal, Revealer} |
%%      {gosterge_finish, Winner}


finish_info(GameMode, FinishReason, Gosterge) ->
    case FinishReason of
        tashes_out -> tashes_out;
        timeout -> timeout;
        set_timeout -> set_timeout;
        {reveal, Revealer, Tashes, Discarded, ConfirmationList} ->
            {RightReveal, RevealWithPairs, WithColor, Hand} = check_reveal(Tashes, Gosterge),
            WinReveal = RightReveal orelse (ConfirmationList /= [] andalso lists:all(fun({_, Answer}) -> Answer == true end, ConfirmationList)),
            TestConfirmation = false,
            if WinReveal orelse TestConfirmation  ->
                   RevealWithColor = case GameMode of
                                         ?MODE_STANDARD -> false;
                                         ?MODE_EVENODD -> WithColor;
                                         ?MODE_COLOR -> WithColor;
                                         ?MODE_COUNTDOWN -> false
                                     end,
                   Okey = gosterge_to_okey(Gosterge),
                   wf:info(?MODULE,"Confirmation List ~p",[ConfirmationList]),
                   RevealWithOkey = Discarded == Okey,
                   
                   WrongRejects = if RightReveal orelse TestConfirmation ->
                                         [S || {S, Answer} <- ConfirmationList, Answer==false];
                                     true -> []
                                  end,
                   wf:info(?MODULE,"Wrong Rejects ~p",[WrongRejects]),
                   {win_reveal, Revealer, WrongRejects, RevealWithColor, RevealWithOkey, RevealWithPairs};
               true ->
                   {fail_reveal, Revealer}
            end;
        {gosterge_finish, Winner} when GameMode == ?MODE_COUNTDOWN ->
            {gosterge_finish, Winner}
    end.


%% @spec get_achivements_points(PointingRules, Achivements) -> AchsPoints
%% @end
get_achivements_points(PointingRules, Achivements) ->
    [{Ach, lists:nth(Ach, PointingRules)} || Ach <- Achivements].

%% @spec sum_achivements_points(AchPoints) -> integer()
%% @end
sum_achivements_points(AchPoints) ->
    lists:foldl(fun({_, P}, Acc)-> Acc + P end, 0, AchPoints).

%% @spec add_delta(TotalScore, RoundScores) -> NewTotalScore
%% @end
add_delta(TotalScore, RoundScores) ->
    [{SeatNum, proplists:get_value(SeatNum, TotalScore) + Delta}
     || {SeatNum, Delta} <- RoundScores].


%% @spec gosterge_to_okey(GostergyTash) -> OkeyTash
%% @end
gosterge_to_okey({Color, Value}) ->
    if Value == 13 -> {Color, 1};
       true -> {Color, Value + 1}
    end.

normalize_reveal([TopRow, BottomRow], Gosterge) ->
    FlatList = TopRow ++ [null | BottomRow],
    Okey = gosterge_to_okey(Gosterge),
    Normalized = [case E of
                      Okey -> okey;
                      false_okey -> Okey;
                       _ -> E
                  end || E <- FlatList],
    Sets = split_by_delimiter(null, Normalized),
    {Sets,Normalized}.

% test [{3,13},{3,11},{1,9},{2,8},{2,13},{3,10},null,{3,8},{4,12},{1,6},{2,12},{1,3},{4,5},{3,5},null,null,{3,2},null,null,null,null,null,null,null,null,null,null,null,null,null,null]

denormalize_reveal(RevealHand, Gosterge) ->
    Okey = gosterge_to_okey(Gosterge),
    Den = [ [ case Tash of
        okey -> Okey;
        Okey -> false_okey;
        _ -> Tash end || Tash <- [null|Series] ] || Series <- RevealHand ],
    {L1,L2}=lists:split(length(RevealHand) div 2,Den),
    [lists:flatten(L1),lists:flatten(L2)].

check_reveal(Tashes, Gosterge) ->
    wf:info(?MODULE,"Check Reveal ~p ~p",[Tashes,Gosterge]),
    {Sets,Normalized} = normalize_reveal(Tashes, Gosterge),
    {RR, RP, RC} = check_manual_reveal({Sets,Normalized}, Gosterge),
    gas:info(?MODULE,"Manual Reveal ~n",[RR]),
    {RightReveal, RevealWithPairs, WithColor, Hand} = case RR of
        false -> gas:info(?MODULE,"Ok We Will Help You~n",[]),
            case okey_reveal:check_reveal(lists:flatten(Sets)) of
                [] -> {false,false,false, []};
                X -> case hd(X) of
                    {x2x7,Pairs} -> {true,true,false, Pairs};
                    {How,Comb} -> 
                        gas:info(?MODULE,"Automatic Reveal ~p ~p~n",[How,Comb]),
                        {true,false,false, Comb} end end;
        true -> {RR, RP, RC, Sets} end.

%% @spec check_reveal(TashPlaces, Gosterge) -> {RightReveal, WithPairs, SameColor}
%% @end
check_manual_reveal({Sets,Normalized}, Gosterge) ->
    gas:info(?MODULE,"check_reveal/2 ~n    ~p ~p~n",[Sets, Gosterge]), 
    ProperHand = lists:all(fun(S) -> is_set(S) orelse is_run(S) end, Sets),
    Pairs = lists:all(fun(S) -> is_pair(S) end, Sets),
    [Color | Rest] = [C || {C, _} <- lists:flatten(Normalized)],
    SameColor = lists:all(fun(C) -> C==Color end, Rest),
    {ProperHand orelse Pairs, Pairs, ProperHand andalso SameColor}.

%% @spec split_by_delimiter(Delimiter, List) -> ListOfList
%% @end
split_by_delimiter(Delimiter, Hand) -> split_by_delimiter(Delimiter, Hand, []).
split_by_delimiter(_, [], Acc) -> lists:reverse(Acc);
split_by_delimiter(Delimiter, [Delimiter | Hand], Acc) -> split_by_delimiter(Delimiter, Hand, Acc);
split_by_delimiter(Delimiter, Hand, Acc) ->
    {L, Rest} = lists:splitwith(fun(X) -> X =/= Delimiter end, Hand),
    split_by_delimiter(Delimiter, Rest, [L | Acc]).

%% @spec is_set(Set) -> boolean()
%% @end
is_set(Set) when
  length(Set) < 3;
  length(Set) > 4 -> false;
is_set(Set) ->
    Normals = [ X || X <- Set, X =/= okey ],
    {_, Value} = hd(Normals),
    SameValue = lists:all(fun({_, V}) -> V == Value end, Normals),
    UniqColors = length(Normals) == length(lists:usort([C || {C, _} <- Normals])),
    SameValue andalso UniqColors.


%% @spec is_run(Set) -> boolean()
%% @end
is_run(Set) when length(Set) < 3 -> false;
is_run(Set) ->
    {Okeys, Normals} = lists:partition(fun(X)-> X == okey end, Set),
    {Color, _} = hd(Normals),
    {Colors, Values} = lists:unzip(Normals),
    SameColor = lists:all(fun(C) -> C == Color end, Colors),
    SortedValues = lists:sort(Values),
    NormalizedValues = if hd(SortedValues)==1 -> tl(SortedValues) ++ [14]; true -> false end,
    OkeysNum = length(Okeys),
    Check1 = check_run(SortedValues, OkeysNum),
    Check2 = check_run(NormalizedValues, OkeysNum),
    SameColor andalso (Check1 orelse Check2).


check_run(false, _) -> false;
check_run([First | Rest], OkeysNum) ->
    check_run(First, Rest, OkeysNum).


check_run(Cur, [Cur | _], _OkeysNum) -> false;
check_run(Cur, [Next | Rest], OkeysNum) when Next == Cur + 1 ->
    check_run(Cur+1, Rest, OkeysNum);
check_run(_Cur, [_Next | _Rest], 0) -> false;
check_run(Cur, [Next | Rest], OkeysNum) ->
    check_run(Cur+1, [Next | Rest], OkeysNum - 1);
check_run(_Cur, [], _OkeysNum) -> true.

%% @spec is_pair(Set) -> boolean()
%% @end
is_pair([_A, okey]) -> true;
is_pair([okey, _B]) -> true;
is_pair([A, A]) -> true;
is_pair(_) -> false.


player_achivements_no_winner(Mode, SeatNum, WhoHasGosterge, Has8Tashes) ->
    player_achivements(Mode, SeatNum, WhoHasGosterge, Has8Tashes, no_winner, undefined,
                       undefined, undefined, undefined, undefined, undefined).

player_achivements_win_reveal(Mode, SeatNum, WhoHasGosterge, Has8Tashes,
                              Revealer, WrongRejects, RevealWithOkey,
                              RevealWithPairs, RevealWithColor) ->
    player_achivements(Mode, SeatNum, WhoHasGosterge, Has8Tashes, reveal,
                       Revealer, WrongRejects, true, RevealWithOkey,
                       RevealWithPairs, RevealWithColor).

player_achivements_fail_reveal(Mode, SeatNum, WhoHasGosterge, Has8Tashes, Revealer) ->
    player_achivements(Mode, SeatNum, WhoHasGosterge, Has8Tashes, reveal,
                       Revealer, [], false, false, false, false).

player_achivements_gosterge_finish(Mode, SeatNum, WhoHasGosterge, Has8Tashes) ->
    player_achivements(Mode, SeatNum, WhoHasGosterge, Has8Tashes, gosterge_finish,
                       undefined, [], false, false, false, false).

%% player_achivements(Mode, SeatNum, WhoHasGosterge, Has8Tashes, FinishType, Revealer, WrongRejects,
%%                    WinReveal, RevealWithOkey, RevealWithPairs, WithColor) -> [{AchId}]
player_achivements(Mode, SeatNum, WhoHasGosterge, Has8Tashes, FinishType, Revealer, WrongRejects,
                   WinReveal, RevealWithOkey, RevealWithPairs, WithColor) ->
    L=[
       %% 1
       {?ACH_GOSTERGE_SHOWN, SeatNum == WhoHasGosterge},
       %% 2
       {?ACH_WIN_REVEAL, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso (not RevealWithPairs) andalso (not WithColor)},
       %% 3
       {?ACH_WIN_REVEAL_WITH_OKEY, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso (not RevealWithPairs) andalso (not WithColor)},
       %% 4
       {?ACH_WIN_REVEAL_WITH_PAIRS, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso RevealWithPairs andalso (not WithColor)},
       %% 5
       {?ACH_WIN_REVEAL_WITH_OKEY_PAIRS, FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso RevealWithPairs andalso (not WithColor)},
       %% 6
       {?ACH_8_TASHES, (Mode == ?MODE_EVENODD orelse Mode == ?MODE_COLOR) andalso FinishType == reveal andalso lists:member(SeatNum, Has8Tashes)},
       %% 7
       {?ACH_WIN_REVEAL_WITH_COLOR, (Mode == ?MODE_EVENODD orelse Mode == ?MODE_COLOR) andalso FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso (not RevealWithPairs) andalso WithColor},
       %% 8
       {?ACH_WIN_REVEAL_WITH_COLOR_OKEY, (Mode == ?MODE_EVENODD orelse Mode == ?MODE_COLOR) andalso FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso (not RevealWithPairs) andalso WithColor},
       %% 9
       {?ACH_WIN_REVEAL_WITH_COLOR_PAIRS, (Mode == ?MODE_EVENODD orelse Mode == ?MODE_COLOR) andalso FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso (not RevealWithOkey) andalso RevealWithPairs andalso WithColor},
       %% 10
       {?ACH_WIN_REVEAL_WITH_COLOR_OKEY_PAIRS, (Mode == ?MODE_EVENODD orelse Mode == ?MODE_COLOR) andalso FinishType == reveal andalso SeatNum == Revealer andalso WinReveal andalso RevealWithOkey andalso RevealWithPairs andalso WithColor},
       %% 11
       {?ACH_FAIL_REVEAL, FinishType == reveal andalso SeatNum == Revealer andalso (not WinReveal)},
       %% 12 AKA others_on_wrong_reveal
       {?ACH_CAUGHT_BLUFF, FinishType == reveal andalso SeatNum =/= Revealer andalso (not WinReveal)},
       %% 13
       {?ACH_EMPTY_BOX, false}, %% XXX: By the last information it is no deduction from players to the chanak
       %% 14
       {?ACH_REJECT_GOOD_HAND, FinishType == reveal andalso lists:member(SeatNum, WrongRejects)},
       %% 15
       {?ACH_GOSTERGE_WINNER, Mode == ?MODE_COUNTDOWN andalso FinishType == gosterge_finish andalso SeatNum == WhoHasGosterge}
      ],
    [Ach || {Ach, true} <- L].

chanak_win_achievements() ->
    [?ACH_WIN_REVEAL_WITH_OKEY,
     ?ACH_WIN_REVEAL_WITH_PAIRS,
     ?ACH_WIN_REVEAL_WITH_OKEY_PAIRS,
     ?ACH_8_TASHES,
     ?ACH_WIN_REVEAL_WITH_COLOR,
     ?ACH_WIN_REVEAL_WITH_COLOR_OKEY,
     ?ACH_WIN_REVEAL_WITH_COLOR_PAIRS,
     ?ACH_WIN_REVEAL_WITH_COLOR_OKEY_PAIRS].

get_pointing_rules(ScoringMode) ->
    {_, Rules} = lists:keyfind(ScoringMode, 1, points_matrix()),
    Rules.

%% TODO: Check the table
points_matrix() ->
    [%%          1   2   3   4   5   6   7   8   9  10  11  12  13  14  15 <--- achievement number
     {standard, [1,  3,  6,  6, 12,  0,  0,  0,  0,  0, -9,  3,  0, -1,  0]},
     {odd,      [1,  3,  6,  6, 12, 12, 24, 48, 48, 96, -9,  3,  0, -1,  0]},
     {even,     [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  0, -1,  0]},
     {ybodd,    [1,  3,  6,  6, 12, 12, 24, 48, 48, 96, -9,  3,  0, -1,  0]},
     {ybeven,   [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  0, -1,  0]},
     {rbodd,    [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  0, -1,  0]},
     {rbeven,   [4, 12, 24, 24, 48, 48, 96,192,192,384,-36, 12,  0, -1,  0]},
     {countdown,[1,  2,  4,  4,  8,  0,  0,  0,  0,  0, -2,  0,  0, -1,  1]}
    ].

get_chanak_refilling_points(ScoringMode) ->
    {_, RefillingPoints} = lists:keyfind(ScoringMode, 1, chanak_refilling_table()),
    RefillingPoints.

chanak_refilling_table() ->
    [
     {standard, 0},
     {odd,      8},
     {even,     8},
     {ybodd,   16},
     {ybeven,  16},
     {rbodd,   16},
     {rbeven,  16},
     {countdown,0}
    ].
%%===================================================================

%% @spec get_scoring_mode(GameMode, Gosterge) ->  ScoringMode
%% @end
get_scoring_mode(?MODE_STANDARD, _) ->  standard;
get_scoring_mode(?MODE_COUNTDOWN, _) -> countdown;
get_scoring_mode(?MODE_COLOR, {Color, Val}) when (Val rem 2) == 0 -> get_scoring_mode_c_even(b2c(Color));
get_scoring_mode(?MODE_COLOR, {Color, Val}) when (Val rem 2) == 1 -> get_scoring_mode_c_odd(b2c(Color));
get_scoring_mode(?MODE_EVENODD, {_Color, Val}) when (Val rem 2) == 0 -> even;
get_scoring_mode(?MODE_EVENODD, {_Color, Val}) when (Val rem 2) == 1 -> odd.

get_scoring_mode_c_odd(C) when C == yellow; C == blue -> ybodd;
get_scoring_mode_c_odd(C) when C == black; C == red -> rbodd.
get_scoring_mode_c_even(C) when C == yellow; C == blue -> ybeven;
get_scoring_mode_c_even(C) when C == black; C == red -> rbeven.

b2c(1) -> red;
b2c(2) -> blue;
b2c(3) -> yellow;
b2c(4) -> black.

run_tests() -> [[F()||{_,F}<-Tests]||{Name,Tests}<-okey_scoring:test_test_()].

%% Tests
test_test_() ->
    [{"win_reveal",
      [
    ?_assertEqual({true,false,true},  check_reveal([[{1,3}, false_okey, {1,4}],[]],{1,3}))
      ]
     },
     {"is_pair",
      [?_assertEqual(true,  is_pair([{1,3}, {1,3}])),
       ?_assertEqual(true,  is_pair([{4,13}, {4,13}])),
       ?_assertEqual(false, is_pair([{4,12}, {4,13}])),
       ?_assertEqual(false, is_pair([{1,1}, {4,8}])),
       ?_assertEqual(true,  is_pair([okey, {3,8}])),
       ?_assertEqual(true,  is_pair([{2,3}, okey])),
       ?_assertEqual(true,  is_pair([okey, okey])),
       ?_assertEqual(false, is_pair([{2,4}, {4,2}, {3,3}])),
       ?_assertEqual(false, is_pair([{2,4}])),
       ?_assertEqual(false, is_pair([okey])),
       ?_assertEqual(false, is_pair([okey, okey, {2,6}]))
      ]},
     {"is_set",
      [?_assertEqual(true,  is_set([{1,3}, {3,3}, {2,3}])),
       ?_assertEqual(true,  is_set([{4,8}, okey, {2,8}])),
       ?_assertEqual(true,  is_set([{4,3}, okey, okey])),
       ?_assertEqual(true,  is_set([{4,13}, {1,13}, {3,13}, {2,13}])),
       ?_assertEqual(true,  is_set([okey, {1,13}, {3,13}, {2,13}])),
       ?_assertEqual(true,  is_set([okey, okey, {3,13}, {2,13}])),
       ?_assertEqual(false, is_set([{2,6}])),
       ?_assertEqual(false, is_set([okey])),
       ?_assertEqual(false, is_set([{3,4}, {2,6}])),
       ?_assertEqual(false, is_set([{2,3}, {4,3}])),
       ?_assertEqual(false, is_set([okey, okey])),
       ?_assertEqual(false, is_set([{3,4}, {1,4}, {3,4}])),
       ?_assertEqual(false, is_set([{3,4}, {1,4}, {2,5}])),
       ?_assertEqual(false, is_set([{3,4}, okey, {2,5}])),
       ?_assertEqual(false, is_set([{2,5}, {3,5}, {4,5}, {1,6}])),
       ?_assertEqual(false, is_set([{2,5}, {3,5}, {4,5}, {2,5}])),
       ?_assertEqual(false, is_set([{2,3}, {3,3}, {4,3}, {1,3}, {3,1}])),
       ?_assertEqual(false, is_set([{2,3}, okey, {4,3}, {1,3}, {3,3}]))
      ]},
     {"is_run",
      [?_assertEqual(false, is_run([{1,3}])),
       ?_assertEqual(false, is_run([okey])),
       ?_assertEqual(false, is_run([{2,2}, {2,3}])),
       ?_assertEqual(false, is_run([okey, {2,3}])),
       ?_assertEqual(false, is_run([{4,1}, {2,3}])),
       ?_assertEqual(true,  is_run([{4,4}, {4,6}, {4,5}])),
       ?_assertEqual(true,  is_run([okey, {4,6}, {4,5}])),
       ?_assertEqual(true,  is_run([okey, {4,6}, okey])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,13}])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,11}, {1,13}])),
       ?_assertEqual(true,  is_run([{2,4}, {2,6}, {2,7}, {2,5}])),
       ?_assertEqual(false, is_run([{2,4}, {1,6}, {2,7}, {2,5}])),
       ?_assertEqual(true,  is_run([{1,12}, {1,1}, {1,11}, {1,10}, {1,13}])),
       ?_assertEqual(false, is_run([{1,12}, {1,1}, {1,11}, {1,11}, {1,13}])),
       ?_assertEqual(false, is_run([{1,12}, {1,1}, {1,2}, {1,11}, {1,13}])),
       ?_assertEqual(true,  is_run([{3,6}, {3,8}, okey, {3,5}, okey])),
       ?_assertEqual(true,  is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}])),
       ?_assertEqual(false, is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}, {3,2}])),
       ?_assertEqual(false, is_run([{3,6}, {3,8}, okey, {3,5}, {3,9}, {1,2}]))
      ]}
    ].
