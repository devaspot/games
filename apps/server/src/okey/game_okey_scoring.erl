%%----------------------------------------------------------------------
%% @author Paul Peregud <paulperegud@gmail.com>
%% @copyright Paynet Internet ve Bilisim Hizmetleri A.S. All Rights Reserved.
%% @doc
%% Scoring for game_okey. Implemented in different thread to isolate complexity.
%% @end
%%-------------------------------------------------------------------
-module(game_okey_scoring).
-behaviour(gen_server).

-export([start_link/1]).
-export([standings/1]).
-export([add_event/3]).
-export([get_chanak_points/1, show_gosterge/4, stats/2, finish_round/4, finish_round/7]).
-export([replace_uid/3, reset_scoring/1, new_chanak/1, set_chanak_points/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("server/include/log.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/game_okey.hrl").
-include_lib("server/include/basic_types.hrl").
-include_lib("db/include/config.hrl").
-include_lib("db/include/accounts.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("server/include/game_tavla.hrl").



-record(okey_stats_entry, {
          uid   :: integer(),
          % move_no   :: integer(),
          reason    :: atom()
         }).
-record(state, {
          %% table data
          game_pid    :: pid(),
          relay_pid   :: pid(),
          mode        :: atom(), %% standard | oddeven | color | countdown
          %% set statistics
          game_no = 0               :: integer(),
          chanak = 0                :: integer(),
          set = []                  :: list(tuple('PlayerId()', integer())),
          round_max                 :: integer(),
          round_cur                 :: integer(),
          set_max                   :: integer(),
          set_cur                   :: integer(),
          history = []              :: list(#'OkeyGameResults'{}),

          %% round statistics
          stats = []                :: list(list(#okey_stats_entry{})),
          gosterge_shown = false    :: boolean(),
          gosterge_finish = false   :: boolean(),

          game_info = []            :: [{atom(), any()}]
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Params) ->
    gen_server:start_link(?MODULE, [Params], []).

add_event(Srv, UId, Reason) ->
    gen_server:cast(Srv, {add_event, UId, Reason}).

reset_scoring(Srv) ->
    gen_server:cast(Srv, reset_scoring).

replace_uid(Srv, UId1, UId2) ->
    gen_server:cast(Srv, {replace_uid, UId1, UId2}).

show_gosterge(Srv, Uid, Gosterge, Hand) ->
    gen_server:call(Srv, {show_gosterge, Uid, Gosterge, Hand}).
-spec finish_round(pid(), #'OkeyGameResults'{}, [{'PlayerId'(),[#'OkeyPiece'{}|null]}], #'OkeyPiece'{}) -> #'OkeyGameResults'{}.
finish_round(Srv, Results, Hands, Gosterge) ->
    finish_round(Srv, Results, Hands, Gosterge, undefined, undefined, undefined).
-spec finish_round(pid(), #'OkeyGameResults'{}, [{'PlayerId'(),[#'OkeyPiece'{}|null]}], #'OkeyPiece'{},
                  'PlayerId'()|undefined, #'OkeyPiece'{}|undefined, [#'OkeyPiece'{}|null]|undefined) -> #'OkeyGameResults'{}.
finish_round(Srv, Results, Hands, Gosterge, Winner, LastRevealTash, RevealHand) ->
    gen_server:call(Srv, {finish_round, Results, Hands, Gosterge, Winner, LastRevealTash, RevealHand}).

get_chanak_points(Srv) ->
    gen_server:call(Srv, get_chanak_points).

set_chanak_points(Srv, Points) ->
    gen_server:call(Srv, {set_chanak_points, Points}).

standings(#'OkeyGameResults'{} = R) ->
    standings0(R).

stats(Srv, Uid) ->
    gen_server:call(Srv, {stats, Uid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initiates the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Params]) ->
    ?INFO("Scoring Params: ~p", [Params]),
    Mode = proplists:get_value(game_mode, Params),
    {is_mode, true} = {is_mode, is_mode(Mode)},
    Sets = proplists:get_value(sets, Params),
    Rounds = proplists:get_value(rounds, Params),
    GF = proplists:get_value(gosterge_finish, Params),
    %% game info filled in game_okey:init/1
    GameInfo = proplists:get_value(game_info, Params),
    game_stats:charge_quota(GameInfo),
    {ok, #state{mode = Mode, gosterge_finish = GF, set_max = Sets,
                set_cur = 1, round_max = Rounds, round_cur = 1,
                game_info = GameInfo}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({show_gosterge, UId, Gosterge, Hand}, _From, #state{gosterge_shown = false} = State) ->
    #state{stats = Stats, mode = Mode, gosterge_finish = GF} = State,
    case {lists:member(Gosterge, Hand), Mode, GF} of
        {true, countdown, true} ->
            Stats2 = [#okey_stats_entry{uid = UId,
                                        reason = gosterge} | Stats],
            {[gosterge], GP} = score_single([gosterge], achievements(countdown, with_scores)),
            Best = get_last_score(UId, State) + GP,
            case Best of
                X when X >= 10 ->
                    Res0 = hd(State#state.history),
                    {RoundRes, GameRes} = countdown_points_reset0(UId, Res0),
                    {reply, {true, RoundRes, GameRes},
                     State#state{gosterge_shown = true, stats = Stats2}};
                _ ->
                    {reply, true, State#state{gosterge_shown = true, stats = Stats2}}
            end;
        {true, _, _} ->
            Stats2 = [#okey_stats_entry{uid = UId,
                                        reason = gosterge} | Stats],
            {reply, true, State#state{gosterge_shown = true, stats = Stats2}};
        {false, _, _} ->
            {reply, false, State}
    end;
handle_call({show_gosterge, _Uid, _Gosterge, _Hand}, _From, #state{gosterge_shown = true} = State) ->
    {reply, false, State};

handle_call({finish_round, _, _, _, _, _, _} = Msg, From, State = #state{set = []}) ->
    Value = State#state.round_cur,
    Stats = State#state.stats,
    Counters = [ {UId, Value} || #okey_stats_entry{uid = UId, reason = Reason} <- Stats, Reason == started ],
    true = (4 =:= length(Counters)),
    State1 = State#state{set = Counters},
    handle_call(Msg, From, State1);
handle_call({finish_round, #'OkeyGameResults'{} = Res0, Hands, Gosterge, undefined, undefined, undefined}, _From, State0) ->
    #state{mode = Mode0, stats = Stats0} = State0,
    Mode = game_okey:get_scoring_mode(Mode0, Gosterge),
    ?INFO("scoring mode: ~p", [Mode]),
    {State1, Stats1} = refill_chanak(Mode, State0, Stats0),
    ?INFO("chanak value: ~p", [State1#state.chanak]),
    Stats = check_hands_for_8_tashes(Hands, Stats1),
    common_round_finish(Stats, Res0, Mode, State1);
handle_call({finish_round, #'OkeyGameResults'{} = Res0, Hands, Gosterge, Winners, LastRevealTash, RevealHand}, _From, State) ->
    #state{mode = Mode0, stats = Stats0} = State,
    Mode = game_okey:get_scoring_mode(Mode0, Gosterge),
    ?INFO("scoring mode: ~p", [Mode]),
    {State1, Stats1} = refill_chanak(Mode, State, Stats0),
    ?INFO("chanak value: ~p", [State1#state.chanak]),
    Stats = check_hands_for_8_tashes(Hands, Stats1),
    [ ?INFO("Player ~p ended with ~p", [binary_to_list(Player),Ending]) || {_,Player,Ending}<-Stats],
    Okey = game_okey:get_okey(Gosterge),
    Stats2 = case Winners of
                 [Winner] ->
                     A = if_list(with_okey(Okey, LastRevealTash),
                                 #okey_stats_entry{uid = Winner#okey_player.player_id, reason = okey}),
                     B = if_list(with_even_tashes(Gosterge, RevealHand),
                                 #okey_stats_entry{uid = Winner#okey_player.player_id, reason = even}),
                     C = if_list(with_color(Gosterge, RevealHand),
                                 #okey_stats_entry{uid = Winner#okey_player.player_id, reason = color}),
                     ?INFO("A: ~p, B: ~p, C: ~p", [A, B, C]),
                     lists:flatten([A, B, C| Stats]);
                 _ ->
                     Stats
             end,
    common_round_finish(Stats2, Res0, Mode, State1);
handle_call(get_chanak_points, _From, State) ->
    {reply, State#state.chanak, State};
handle_call({set_chanak_points, Points}, _From, State) ->
    {reply, ok, State#state{chanak = Points}};
handle_call(_Request, _From, State) ->
    {reply, {error, {unknown_call, _Request}}, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({add_event, UId, Reason}, State) ->
    #state{stats = Stats} = State,
    Stats2 = [#okey_stats_entry{uid = UId, reason = Reason} | Stats],
    {noreply, State#state{stats = Stats2}};

handle_cast({replace_uid, UId1, UId2}, State) ->
    #state{stats = Stats, history = History} = State,
    Stats2 = lists:map(fun
                           (#okey_stats_entry{uid = UId} = X) when UId == UId1 ->
                               X#okey_stats_entry{uid = UId2};
                           (X) ->
                               X
                       end, Stats),
    Set2 = lists:map(fun
                          ({UId, Val}) when UId == UId1 ->
                              {UId2, Val};
                          (X) ->
                              X
                      end, Stats),
    F2 = fun
             (#'OkeyGameR'{player_id = X} = R) when X == UId1 ->
                 R#'OkeyGameR'{player_id = UId2};
             (R) ->
                 R
         end,
    F1 = fun(#'OkeyGameResults'{results = RRs} = R) ->
                 RRs2 = lists:map(F2, RRs),
                 R#'OkeyGameResults'{results = RRs2}
         end,
    History2 = lists:map(F1, History),
    {noreply, State#state{stats = Stats2, set = Set2, history = History2}};

handle_cast(update_to_next_round_or_set, State) ->
    {noreply, update_to_next_round_or_set(State)};

handle_cast(reset_scoring, State) ->
    {noreply, reset_scoring0(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
% handle_info(die, State) ->
%     {stop, normal, State};
handle_info(Info, State) ->
    ?INFO("unrecognized info: ~p", [Info]),
    {stop, {error, unrecognized_info}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_last_score(UId, State) ->
    case State#state.history of
        [] ->
            0;
        [H | _] ->
            RRs = H#'OkeyGameResults'.results,
            R = lists:keyfind(UId, #'OkeyGameR'.player_id, RRs),
            R#'OkeyGameR'.score
    end.

gosterge_finish_round_result(UId, RoundScore, #'OkeyGameResults'{results = RRs0} = Res0) ->
    RRs1 = lists:map(fun(X) ->
                             X#'OkeyGameR'{
                               score_delta = 0,
                               breakdown = [],
                               skill_delta = 0
                              }
                     end, RRs0),
    {value, R, RRs2} = lists:keytake(UId, #'OkeyGameR'.player_id, RRs1),
    R1 = R#'OkeyGameR'{
           winner = true,
           good_shot = true,
           skill_delta = 1,
           score_delta = RoundScore,
           breakdown = [#'OkeyScoringDetail'{reason = [gosterge],
                                             score = RoundScore}]
          },
    Res0#'OkeyGameResults'{results = [R1 | RRs2]}.

gosterge_finish_game_result(UId, GameScore, Res0) ->
    #'OkeyGameResults'{results = RRs1} = strip_gamepoints(Res0),
    {value, R, RRs2} = lists:keytake(UId, #'OkeyGameR'.player_id, RRs1),
    R1 = R#'OkeyGameR'{
           winner = true,
           good_shot = true,
           skill_delta = 1,
           score_delta = GameScore,
           score = GameScore,
           breakdown = [#'OkeyScoringDetail'{reason = [gosterge_winner],
                                             score = GameScore}]
          },
    Res0#'OkeyGameResults'{results = [R1 | RRs2]}.

positions_arrange(Rs0) ->
    Rs = lists:reverse(lists:keysort(#'OkeyGameR'.score, Rs0)),
    [#'OkeyGameR'{score = MaxScore} | _] = Rs,
    positions_arrange(1, MaxScore, Rs).

positions_arrange(_, _, []) ->
    [];
positions_arrange(P, Score, [R | Rest]) ->
    case R of
        #'OkeyGameR'{score = Score} ->
            [{P, R} | positions_arrange(P, Score, Rest) ];
        #'OkeyGameR'{score = LowerScore} ->
            [{P+1, R} | positions_arrange(P+1, LowerScore, Rest) ]
    end.

standings0(#'OkeyGameResults'{results = Rs0}) ->
    Rs = positions_arrange(Rs0),
    [ begin
          #'OkeyGameR'{player_id = UId, score = Score} = R,
          #'OkeySeriesResult'{player_id = UId, place = Position, score = Score, 
                     winner = case Position =:= 1 of true -> <<"true">>; false -> <<"none">> end }
      end
      || {Position, R} <- Rs ].

strip_gamepoints(#'OkeyGameResults'{results = R} = Res) ->
    R2 = [ strip_gamepoints(R0) || R0 <- R],
    Res#'OkeyGameResults'{results = R2};
strip_gamepoints(#'OkeyGameR'{} = R) ->
    R#'OkeyGameR'{
      winner = false,
      good_shot = false,
      score_delta = 0,
      skill_delta = 0,
      score = 0,
      breakdown = []
     }.

countdown_points_reset(false, Res) ->
    {Res, Res};
countdown_points_reset({true, PlayerId}, Res) ->
    countdown_points_reset0(PlayerId, Res).

countdown_points_reset0(PlayerId, Res) ->
    {[gosterge], RoundScore} = score_single([gosterge], achievements(countdown, with_scores)),
    {[gosterge_winner], GameScore} = score_single([gosterge_winner], achievements(countdown, with_scores)),
    RoundRes = gosterge_finish_round_result(PlayerId, RoundScore, Res),
    GameRes = gosterge_finish_game_result(PlayerId, GameScore, Res),
    {RoundRes, GameRes}.

update_history(Res, State) ->
    Spin = fun(#'OkeyGameResults'{results = List}) ->
                   lists:map(fun(X) -> {X#'OkeyGameR'.score_delta, X#'OkeyGameR'.score} end, List)
           end,
    case State#state.history of
        [] ->
            Res1 = trim_to_10(Res, State#state.mode),
            ?INFO("old entry: none", []),
            ?INFO("new entry: ~p", [Spin(Res1)]),
            ?INFO("cmb entry: ~p", [Spin(Res1)]),
            {State#state{history = [Res1]}, Res1};
        [H|Rest] ->
            C = combine(Res, H, State#state.mode),
            ?INFO("old entry: ~p", [Spin(H)]),
            ?INFO("new entry: ~p", [Spin(Res)]),
            ?INFO("cmb entry: ~p", [Spin(C)]),
            {State#state{history = [C, H | Rest]}, C}
    end.

combine(Base = #'OkeyGameResults'{}, History = #'OkeyGameResults'{}, Mode) ->
    BaseR = Base#'OkeyGameResults'.results,
    HistoryR = History#'OkeyGameResults'.results,
    ComulativeRes = [ combine(B, H, Mode) || B = #'OkeyGameR'{player_id = UIdB} <- BaseR,
                                             H = #'OkeyGameR'{player_id = UIdH} <- HistoryR,
                                             UIdB == UIdH ],
    Base#'OkeyGameResults'{results = ComulativeRes};
combine(B = #'OkeyGameR'{}, H = #'OkeyGameR'{}, Mode) when Mode == countdown->
    trim_to_10(combine(B, H), Mode);
combine(B = #'OkeyGameR'{}, H = #'OkeyGameR'{}, _Mode) ->
    combine(B, H).
combine(B = #'OkeyGameR'{}, H = #'OkeyGameR'{}) ->
    BPD = B#'OkeyGameR'.score_delta,
    HPD = H#'OkeyGameR'.score,
    B#'OkeyGameR'{score = BPD + HPD}.

trim_to_10(Results = #'OkeyGameResults'{}, Mode) when Mode == countdown ->
    RRs = Results#'OkeyGameResults'.results,
    RRs1 = [ trim_to_10(R, Mode) || R <- RRs ],
    Results#'OkeyGameResults'{results = RRs1};
trim_to_10(Results = #'OkeyGameResults'{}, _Mode)  ->
    Results;
trim_to_10(S = #'OkeyGameR'{score = Score}, Mode) when Score < 0, Mode == countdown ->
    S#'OkeyGameR'{score = 0};
trim_to_10(S, _Mode) ->
    S.

get_set_state(State) ->
    #'OkeySetState'{round_cur = State#state.round_cur,
                    round_max = State#state.round_max,
                    set_cur = State#state.set_cur,
                    set_max = State#state.set_max}.


get_max_countdown_counter(#state{history = [L|_]} = _State) ->
    #'OkeyGameResults'{results = Rs} = L,
    MaxP = lists:last(lists:keysort(#'OkeyGameR'.score, Rs)),
    {MaxP#'OkeyGameR'.player_id, MaxP#'OkeyGameR'.score}.

%% begin CODE for countdown
check_set_ending(#state{mode = countdown, round_cur = RCur,
                        set_cur = SetCur, set_max = SetMax} = State) ->
    {A, B} = get_max_countdown_counter(State),
    ?INFO("SetCur: ~p, SetMax: ~p", [SetCur, SetMax]),
    ?INFO("coundown best score: ~p", [{A, B}]),
    case {A, B} of
        {PlayerId, Points} when Points >= 10 ->
            Acc =
                case SetCur of
                    SetMax -> done; %% it was last set
                    _ ->      next_set %% there are few more sets left
                end,
            %% PlayerResults2 = avard_countdown_bonus(PlayerId, PlayerResults),
            {State#state{round_cur = 1,
                         set_cur = SetCur + 1,
                         stats = [],
                         gosterge_shown = false},
             Acc,
             {true, PlayerId}};
        {_PlayerId, Points} when Points < 10 ->
            {State#state{round_cur = RCur + 1, stats = [], gosterge_shown = false},
             next_round,
             false}
    end;%% end CODE for countdown


check_set_ending(State = #state{round_cur = RCur0, round_max = RMax0,
                                set_cur = SetCur0, set_max = SetMax0})
  when (RCur0 /= RMax0) ->
    ?INFO("check_set_ending A, next_round. debug: ~p", [{RCur0, RMax0, SetCur0, SetMax0}]),
    State1 = update_to_next_round_or_set(State),
    {State1, next_round, false};
check_set_ending(State = #state{set_cur = SetCur0, set_max = SetMax0,
                                round_cur = RCur0, round_max = RMax0})
  when (RCur0 == RMax0) andalso (SetCur0 < SetMax0)->
    ?INFO("check_set_ending A, next_set. debug: ~p", [{RCur0, RMax0, SetCur0, SetMax0}]),
    State1 = update_to_next_round_or_set(State),
    {State1, next_set, false};
check_set_ending(State = #state{set_cur = SetCur, set_max = SetMax,
                                round_cur = RCur, round_max = RMax}) ->
    ?INFO("check_set_ending B, done. debug: ~p", [{RCur, RMax, SetCur, SetMax}]),
    State1 = update_to_next_round_or_set(State),
    {State1, done, false}.

update_to_next_round_or_set(State = #state{mode = Mode}) when Mode == standard; Mode == evenodd; Mode == color ->
    NextRound = State#state.round_cur + 1,
    CurrentSet = State#state.set_cur,
    ?INFO("update_to_next_round_or_set. {set_cur, round_cur}: ~p", [{State#state.set_cur, State#state.round_cur}]),
    case NextRound > State#state.round_max of
        true ->
            %% new set
            ?INFO("next set", []),
            State#state{round_cur = 1, set_cur = CurrentSet + 1, stats = [], gosterge_shown = false};
        false ->
            ?INFO("next round", []),
            %% new round
            State#state{round_cur = NextRound, stats = [], gosterge_shown = false}
    end;
update_to_next_round_or_set(State) -> %% case when Mode = countdown
    State#state{stats = [], gosterge_shown = false}.


check_hands_for_8_tashes(Hands, Stats) ->
    lists:foldl(fun({UId, H}, Acc) ->
                        case with_8_tashes(H) of
                            true ->
                                A = #okey_stats_entry{uid = UId, reason = with_8_tashes},
                                [A | Acc];
                            false ->
                                Acc
                        end
                end, Stats, Hands).

reset_scoring0(State) ->
    ?INFO("External reset scoring. set 1, round 1", []),
    State#state{round_cur = 1, set_cur = 1, stats = [], gosterge_shown = false, history = []}.

common_round_finish(Stats, Res0, Mode, State1) ->
    Res2 = analyze_game(Stats, Res0, Mode),
    {State2, Res4} = update_set(Mode, State1, Res2),
    {State3, Res5} = update_history(Res4, State2),
    {State4, IsLast, CountdownReset} = check_set_ending(State3),
    RoundResults = translate_reasons(Res5),
    {_, GameResults0} = countdown_points_reset(CountdownReset, Res5),
    case IsLast of
        done ->
            GameResults = translate_reasons(GameResults0),
            %game_stats:assign_points(GameResults, State1#state.game_info),
            {reply, {ok, {done, GameResults},                 RoundResults}, State4#state{history = []}};
        next_set ->
            {reply, {ok, {next_set,   get_set_state(State4)}, RoundResults}, State4#state{history = []}};
        next_round ->
            {reply, {ok, {next_round, get_set_state(State4)}, RoundResults}, State4}
    end.

%% REWRITE THIS PART!!!
update_set(Mode, State, R) when Mode =/= standard ->
    #'OkeyGameResults'{results = Res} = R,
    %% find reveal, if available
    FilterFun = fun(#'OkeyScoringDetail'{reason = X}) ->
                        length(X) > 1 andalso lists:member(reveal, X) %% reason is [reveal, ..]
                end,
    CW = find_first_ogr(FilterFun, Res),
    case CW of
        null ->
            update_set2(State, R);
        _ ->
            update_set_state(State, R, CW)
    end;
update_set(_, State, R) ->
    {State, R}.

update_set2(State = #state{}, R) ->
    #'OkeyGameResults'{results = Res} = R,
    %% find with_8_tashes, if available
    FilterFun = fun(#'OkeyScoringDetail'{reason = X}) ->
                        lists:member(with_8_tashes, X) %% reason is [with_8_tashes, ..]
                end,
    CW = find_first_ogr(FilterFun, Res),
    case CW of
        null ->
            {State, R};
        _ ->
            update_set_state(State, R, CW)
    end.

find_first_ogr(FilterFun, Res) ->
    lists:foldl(fun
                    (#'OkeyGameR'{breakdown = BR} = OGR, null) ->
                        P = lists:filter(FilterFun, BR),
                        case P of
                            [] -> null;
                            _ -> OGR
                        end;
                    (_, Acc) ->
                             Acc
                end, null, Res).

update_set_state(State, R = #'OkeyGameResults'{}, CW) ->
    Entries0 = R#'OkeyGameResults'.results,
    Entries1 = lists:delete(CW, Entries0),
    
    Reward = State#state.chanak,
    #'OkeyGameR'{breakdown = BR, score = OrgScore, score_delta = OrgDelta} = CW,
    ?INFO("chanak points: ~p", [Reward]),
    BR1 = [#'OkeyScoringDetail'{reason = <<"chanak points">>, score = Reward} | BR],

    Entries2 = [CW#'OkeyGameR'{breakdown = BR1,
                               score = OrgScore + Reward,
                               score_delta = OrgDelta + Reward} | Entries1],
    {State#state{chanak = 0}, R#'OkeyGameResults'{results = Entries2}}.

refill_chanak(Mode, State = #state{gosterge_shown = X}, Stats0) when X == false, Mode =/= standard, Mode =/= countdown ->
    Chanak = State#state.chanak,
    L = [ #okey_stats_entry{uid = UId, reason = box_empty} || {UId, _} <- State#state.set ],
    Achievements = achievements(Mode, with_scores),
    {_, Deduction} = lists:keyfind([box_empty], 1, Achievements),
    Stats = %L ++ 
            Stats0,
    NewChanak = Chanak + Deduction,
    {State#state{chanak = NewChanak, stats = Stats}, Stats};
refill_chanak(_, State, Stats) ->
    {State, Stats}.

new_chanak(Mode) ->
    Achievements = achievements(Mode, with_scores),
    {_, Deduction} = lists:keyfind([gosterge_winner], 1, Achievements),
    Deduction.

analyze_game(Stats, #'OkeyGameResults'{results = PlayerResults0} = Res, Mode) ->
    PlayerEvents = dict:from_list([ {Id, []} || #'OkeyGameR'{player_id = Id} <- PlayerResults0 ]),
%    ?INFO("PlayerEvents: ~p", [PlayerEvents]),
    PlayerEvents1 = analyze_game0(Stats, PlayerEvents, fun describe_achievement/2),
%    ?INFO("PlayerEvents1: ~p", [PlayerEvents1]),
    Check = [ check_stats(X) ||  {_Id, X} <- dict:to_list(PlayerEvents1) ],
    case lists:any(fun(Bool) -> Bool end, Check) of
        false ->
%            ?INFO("PlayerEvents: ~p", [PlayerEvents1]),
            erlang:error(incomplete_stats_detected);
        true -> ok
    end,
    PEL = dict:to_list(PlayerEvents1),
    PlayerEvents2 = [ {Id, ordsets:from_list(X) } ||  {Id, X} <- PEL ],
    AMode = achievements(Mode),
    Achievements = ordsets:from_list(AMode),
    Reduced = [ {Id, reduce_ach(match_ach(PE, Achievements))} || {Id, PE} <- PlayerEvents2 ],
    Scored = [ score_ach(R, RA, Mode)
               || {{_Id, RA}, R} <- lists:zip(lists:keysort(1, Reduced),
                                              lists:keysort(#'OkeyGameR'.player_id, PlayerResults0)) ],
    Res#'OkeyGameResults'{results = Scored}.

match_ach(MyList, OffList) ->
    lists:filter(fun(X) -> ordsets:is_subset(X, MyList) end, OffList).


reduce_ach(L) ->
    reduce_ach(L, L, []).
reduce_ach([], _L, Acc) ->
    Acc;
reduce_ach([H | T], L, Acc) ->
    case lists:any(fun
                       (X) when X =:= H -> false;
                       (X) -> ordsets:is_subset(H, X)
                   end, L) of
        true ->
            reduce_ach(T, L, Acc);
        false ->
            reduce_ach(T, L, [H | Acc])
    end.

score_ach(R, [], _Mode) ->
    R;
score_ach(#'OkeyGameR'{score_delta = PD} = R, [H0|T] = _RA, Mode) ->
    Achievements = achievements(Mode, with_scores),
    case score_single(H0, Achievements) of
        false ->
            score_ach(R, T, Mode);
        {H, Score} ->
            BD = R#'OkeyGameR'.breakdown,
            A = #'OkeyScoringDetail'{reason = H, score = Score},
            R1 = R#'OkeyGameR'{score_delta = PD + Score,
                               score = PD + Score,
                               breakdown = [A | BD]},
            score_ach(R1, T, Mode)
    end.

score_single(Entry, Achievements) ->
    H = lists:usort(ordsets:to_list(Entry)),
    lists:keyfind(H, 1, Achievements).

translate_reasons(#'OkeyGameResults'{results = R} = Res) ->
    R2 = [ translate_reasons(X) || X  <- R ],
    Res#'OkeyGameResults'{results = R2};
translate_reasons(#'OkeyGameR'{breakdown = R} = Res) ->
    R2 = [ translate_reasons(X) || X  <- R ],
    Res#'OkeyGameR'{breakdown = R2};
translate_reasons(#'OkeyScoringDetail'{reason = R} = Res) when is_binary(R) ->
    Res;
translate_reasons(#'OkeyScoringDetail'{reason = R} = Res) ->
    Descs = lists:zip(achievements(), achievements_desc()),
    {R, Reason} = lists:keyfind(R, 1, Descs),
    Res#'OkeyScoringDetail'{reason = Reason}.

analyze_game0([], Players, _F) ->
    Players;
analyze_game0([#okey_stats_entry{uid = UId, reason = Reason} | Rest], Players, F) ->
    Player = dict:fetch(UId, Players),
    Player2 = F(Reason, Player),
    analyze_game0(Rest, dict:store(UId, Player2, Players), F).

describe_achievement(Event, List) ->
    [Event | List].

with_okey(Okey, Okey) ->
    true;
with_okey(_Okey, _LastTash) ->
    false.

with_even_tashes(Gosterge, Hand0) ->
    Okey = game_okey:get_okey(Gosterge),
    Hand = game_okey:normalize_hand(null, Hand0),
    Rev2 = lists:map(fun(J) when J == Okey -> okey;
                        (Other) -> Other
                     end, Hand),
    Rev3 = lists:map(fun(?FALSE_OKEY) -> Okey;
                        (Other) -> Other
                     end, Rev2),
    Sets = game_okey:split_by_delimiter(null, Rev3),
    Res = lists:all(fun(S) ->
                      game_okey:is_pair(S)
              end, Sets),
    Res andalso ?INFO("Detected even tashes, gosterge: ~p~nhand ~p", [Gosterge, Hand0]),
    Res.

with_8_tashes(Hand0) ->
    Hand = game_okey:normalize_hand(null, Hand0),
    Sets = game_okey:split_by_delimiter(null, Hand),
    Pairs0 = lists:filter(fun
                              ([A, A]) -> true;
                              (_) -> false
                          end, Sets),
    Singles = lists:map(fun(L) -> hd(L) end, Pairs0),
    case length(Singles) of
        X when X > 3 ->
            check_combinations(Singles);
        _ ->
            false
    end.

with_color(Gosterge, Hand) ->
    Okey = game_okey:get_okey(Gosterge),
    Tashes0 = lists:flatten(game_okey:split_by_delimiter(null, game_okey:normalize_hand(null, Hand))),
    Tashes1 = lists:filter(fun(J) when J == Okey -> false;
                              (_) -> true
                           end, Tashes0),
    Tashes = lists:map(fun(J) when J == ?FALSE_OKEY -> Okey;
                          (J) -> J
                       end, Tashes1),
    TheC = (hd(Tashes))#'OkeyPiece'.color,
    lists:all(fun
                  (#'OkeyPiece'{color = C}) when C == TheC -> true;
                  (#'OkeyPiece'{color = _}) -> false
              end, Tashes).

check_combinations(Singles) ->
    Combs = kakamath:combination(4, Singles),
    lists:any(fun(List) ->
                      same_values(List)
              end, Combs).

same_values(List) ->
    Val = (hd(List))#'OkeyPiece'.value,
    lists:all(fun
                  (#'OkeyPiece'{value = V}) when V == Val -> true;
                  (_) -> false
              end, List).


if_list(true, X) ->
    [X];
if_list(false, _) ->
    [].

is_mode(Mode) ->
    lists:member(Mode, [standard, evenodd, color, countdown]).

achievements(Mode, with_scores) ->
    GP = game_points(),
    {Mode, Points} = lists:keyfind(Mode, 1, GP),
    Res0 = lists:zip(achievements(), Points),
    Res1 = [ P || {_, Score} = P <- Res0, Score =/= 0 ],
    Res1.
achievements(Mode) ->
    {Ach, _Scores} = lists:unzip(achievements(Mode, with_scores)),
    Ach.

achievements() ->
   A = [
        [gosterge],                  %% 1
        [reveal],                    %% 2
        [reveal, okey],              %% 3
        [reveal, even],              %% 4
        [reveal, even, okey],        %% 5
        [no_reveal, with8tashes],    %% 6
        [reveal, color],             %% 7
        [reveal, color, okey],       %% 8
        [reveal, color, even],       %% 8
        [reveal, color, even, okey], %% 10
        [wrong_reveal],              %% 11
        [caught_bluff],              %% 12 %% AKA others_on_wrong_reveal
        [box_empty],                 %% 13
        [rejected_good_hand],        %% 14
        [gosterge_winner]            %% 15
       ],%% note: those points are added
    [ lists:usort(X) || X <- A ].
achievements_desc() ->
    [
     <<"gosterge shown">>,                            %% 1
     <<"proper reveal">>,                             %% 2
     <<"reveal with okey">>,                          %% 3
     <<"reveal with even tashes">>,                   %% 4
     <<"reveal with even tashes and okey">>,          %% 5
     <<"8 tashes of value in hand">>,                 %% 6
     <<"reveal with color">>,                         %% 7
     <<"reveal with color and okey">>,                %% 8
     <<"reveal with color and even tashes">>,         %% 9
     <<"reveal with color, even tashes and okey">>,   %% 10
     <<"reveal with wrong hand">>,                    %% 11
     <<"caught bluff">>,                              %% 12 %% AKA others_on_wrong_reveal
     <<"deduction to empty box">>,                    %% 13
     <<"rejected good hand">>,                        %% 14
     <<"gosterge winner">>                            %% 15
    ].%% note: those points are added


%% note - all penalties/deductions should be expressed here as negative numbers
game_points() ->
    [%%          1   2   3   4   5   6   7   8   9  10  11  12  13  14, 15 <--- achievement number
     {standard, [1,  3,  6,  6, 12,  0,  0,  0,  0,  0, -9,  3,  1,  0,  1]},
     {odd,      [1,  3,  6,  6, 12, 12, 24, 48, 48, 96, -9,  3,  1,  0,  8]},
     {even,     [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  2,  0,  8]},
     {ybodd,    [1,  3,  6,  6, 12, 12, 24, 48, 48, 96, -9,  3,  1,  0, 16]},
     {ybeven,   [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  2,  0, 16]},
     {rbodd,    [2,  6, 12, 12, 24, 24, 48, 96, 96,192,-18,  6,  2,  0, 16]},
     {rbeven,   [4, 12, 24, 24, 48, 48, 96,192,192,384,-36, 12,  4,  0, 16]},
     {countdown,[1,  2,  4,  4,  8,  0,  0,  0,  0,  0, -2,  0,  1,  0,  1]}
    ].%% note: those points are added


%% debug
check_stats(List) ->
    lists:member(reveal, List)
        orelse lists:member(wrong_reveal, List)
        orelse lists:member(disconnected, List)
        orelse lists:member(out_of_tashes, List).

%%%===================================================================
%%% Tests
%%%===================================================================

detecting_8_tashes_test() ->
    H = [ {true, [#'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 2},
                  null,
                  #'OkeyPiece'{color = 2, value = 2},
                  #'OkeyPiece'{color = 2, value = 2},
                  null,
                  #'OkeyPiece'{color = 3, value = 2},
                  #'OkeyPiece'{color = 3, value = 2},
                  null,
                  #'OkeyPiece'{color = 4, value = 1},
                  #'OkeyPiece'{color = 4, value = 3},
                  null,
                  #'OkeyPiece'{color = 4, value = 2},
                  #'OkeyPiece'{color = 4, value = 2},
                  null,
                  #'OkeyPiece'{color = 2, value = 12},
                  #'OkeyPiece'{color = 3, value = 7},
                  #'OkeyPiece'{color = 2, value = 7},
                  #'OkeyPiece'{color = 2, value = 12}
                 ], #'OkeyPiece'{color = 2, value = 12}},
          {false, [#'OkeyPiece'{color = 1, value = 2},
                   #'OkeyPiece'{color = 1, value = 2},
                   null,
                   #'OkeyPiece'{color = 2, value = 2},
                   #'OkeyPiece'{color = 2, value = 2},
                   null,
                   #'OkeyPiece'{color = 3, value = 2},
                   #'OkeyPiece'{color = 3, value = 2},
                   null,
                   #'OkeyPiece'{color = 4, value = 1},
                   #'OkeyPiece'{color = 4, value = 3},
                   null,
                   #'OkeyPiece'{color = 4, value = 2},
                   #'OkeyPiece'{color = 2, value = 12},
                   null,
                   #'OkeyPiece'{color = 2, value = 12},
                   #'OkeyPiece'{color = 3, value = 7},
                   #'OkeyPiece'{color = 2, value = 7},
                   #'OkeyPiece'{color = 2, value = 12}
                  ], #'OkeyPiece'{color = 2, value = 12}},
          {false, [#'OkeyPiece'{color = 1, value = 2},
                   #'OkeyPiece'{color = 1, value = 2},
                   null,
                   #'OkeyPiece'{color = 2, value = 2},
                   ?FALSE_OKEY,
                   null,
                   #'OkeyPiece'{color = 3, value = 2},
                   #'OkeyPiece'{color = 3, value = 2},
                   null,
                   #'OkeyPiece'{color = 4, value = 1},
                   #'OkeyPiece'{color = 4, value = 3},
                   null,
                   #'OkeyPiece'{color = 4, value = 2},
                   #'OkeyPiece'{color = 2, value = 12},
                   null,
                   #'OkeyPiece'{color = 2, value = 12},
                   #'OkeyPiece'{color = 3, value = 7},
                   #'OkeyPiece'{color = 2, value = 7},
                   #'OkeyPiece'{color = 2, value = 12}
                  ], #'OkeyPiece'{color = 2, value = 2}},
          {false, [#'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 2, value = 5},
                  #'OkeyPiece'{color = 2, value = 5},
                  null,
                  null,
                  #'OkeyPiece'{color = 3, value = 7},
                  #'OkeyPiece'{color = 3, value = 7},
                  null,
                  #'OkeyPiece'{color = 4, value = 7},
                  #'OkeyPiece'{color = 4, value = 7},
                  null,
                  #'OkeyPiece'{color = 2, value = 1},
                  #'OkeyPiece'{color = 2, value = 1},
                  null,
                  #'OkeyPiece'{color = 2, value = 12},
                  #'OkeyPiece'{color = 3, value = 7},
                  #'OkeyPiece'{color = 2, value = 7},
                  #'OkeyPiece'{color = 2, value = 12}
                 ], #'OkeyPiece'{color = 2, value = 12}},
          {false, [#'OkeyPiece'{color = 1, value = 2},
                   #'OkeyPiece'{color = 1, value = 3},
                   #'OkeyPiece'{color = 1, value = 4},
                   #'OkeyPiece'{color = 1, value = 6},
                   null,
                   #'OkeyPiece'{color = 1, value = 5},
                   #'OkeyPiece'{color = 2, value = 5},
                   #'OkeyPiece'{color = 3, value = 5},
                   #'OkeyPiece'{color = 4, value = 5},
                   null,
                   #'OkeyPiece'{color = 2, value = 1},
                   #'OkeyPiece'{color = 2, value = 13},
                   #'OkeyPiece'{color = 2, value = 12},
                   null,
                   #'OkeyPiece'{color = 3, value = 7},
                   #'OkeyPiece'{color = 2, value = 7},
                   #'OkeyPiece'{color = 2, value = 12}
                  ], #'OkeyPiece'{color = 2, value = 12}}
         ],
    HS = lists:zip(lists:seq(1, length(H)), H),
    lists:map(fun({Num, {ProperResult, Hand, _}}) ->
                      Z = {Num, with_8_tashes(Hand), Hand},
                      {Num, ProperResult, Hand} = Z
              end, HS).

detecting_color_test() ->
    H = [ {true, [#'OkeyPiece'{color = 1, value = 1},
                  #'OkeyPiece'{color = 1, value = 2},
                  #'OkeyPiece'{color = 1, value = 3},
                  #'OkeyPiece'{color = 1, value = 4},
                  null,
                  #'OkeyPiece'{color = 1, value = 5},
                  #'OkeyPiece'{color = 1, value = 6},
                  #'OkeyPiece'{color = 1, value = 7},
                  #'OkeyPiece'{color = 1, value = 8},
                  null,
                  #'OkeyPiece'{color = 1, value = 8},
                  #'OkeyPiece'{color = 1, value = 9},
                  #'OkeyPiece'{color = 1, value = 11},
                  #'OkeyPiece'{color = 1, value = 12},
                  #'OkeyPiece'{color = 1, value = 13},
                  #'OkeyPiece'{color = 2, value = 12}
                 ], #'OkeyPiece'{color = 2, value = 11}},
          {false, [#'OkeyPiece'{color = 1, value = 1},
                   #'OkeyPiece'{color = 1, value = 2},
                   #'OkeyPiece'{color = 1, value = 3},
                   #'OkeyPiece'{color = 1, value = 4},
                   null,
                   #'OkeyPiece'{color = 2, value = 5},
                   #'OkeyPiece'{color = 2, value = 6},
                   #'OkeyPiece'{color = 2, value = 7},
                   #'OkeyPiece'{color = 2, value = 8},
                   null,
                   #'OkeyPiece'{color = 1, value = 8},
                   #'OkeyPiece'{color = 1, value = 9},
                   #'OkeyPiece'{color = 1, value = 11},
                   #'OkeyPiece'{color = 1, value = 12},
                   #'OkeyPiece'{color = 1, value = 13},
                   #'OkeyPiece'{color = 2, value = 12}
                  ], #'OkeyPiece'{color = 2, value = 11}},
          {false, [[null,
                    {'OkeyPiece',1,2},
                    {'OkeyPiece',3,2},
                    {'OkeyPiece',1,3},
                    {'OkeyPiece',3,5},
                    {'OkeyPiece',4,5},
                    {'OkeyPiece',1,7},
                    {'OkeyPiece',3,7},
                    {'OkeyPiece',4,7},
                    {'OkeyPiece',2,8},
                    {'OkeyPiece',4,8},
                    {'OkeyPiece',3,9},
                    {'OkeyPiece',3,10},
                    {'OkeyPiece',1,11}],
                   [{'OkeyPiece',1,12},
                    null,null,null,null,null,null,null,null,null,
                    null,null,null,null]], {'OkeyPiece',1,0}}

        ],
    HS = lists:zip(lists:seq(1, length(H)), H),
    lists:map(fun({Num, {ProperResult, Hand, Gosterge}}) ->
                      Z = {Num, with_color(Gosterge, Hand), Hand, Gosterge},
                      {Num, ProperResult, Hand, Gosterge} = Z
              end, HS).

reduce_ach_test() ->
    H0 = [
          { [[a]],
            [[a],[]] },

          { [[a,b]],
            [[a],[b],[a,b]] },

          { [[a,b],[a,c]],
            [[a],[b],[a,b],[a,c]] },

          { [[a,b],[c]],
            [[a],[],[c],[b],[a,b]] }
         ],
    H = [ { [ ordsets:from_list(AX) || AX <- A ], [ ordsets:from_list(BX) || BX <- B ]} || {A, B} <- H0 ],
    HS = lists:zip(lists:seq(1, length(H)), H),
    lists:map(fun({Num, {Ideal0, Fresh}}) ->
                      Reduced = reduce_ach(Fresh),
                      Z = {Num, ordsets:from_list(Reduced), Fresh},
                      Ideal = ordsets:from_list(Ideal0),
                      {Num, Ideal, Fresh} = Z
              end, HS).

insert_at_random_test() ->
    A = put_some_nulls(20, lists:seq(1, 10)),
    ?INFO("insert_at_random_test: ~p", [A]).


positions_arrange_test() ->
    [{1, #'OkeyGameR'{player_id = 1}},
     {2, #'OkeyGameR'{player_id = 2}},
     {3, #'OkeyGameR'{player_id = 3}},
     {4, #'OkeyGameR'{player_id = 4}}]
        = positions_arrange([#'OkeyGameR'{score = 40, player_id = 1},
                          #'OkeyGameR'{score = 30, player_id = 2},
                          #'OkeyGameR'{score = 20, player_id = 3},
                          #'OkeyGameR'{score = 10, player_id = 4}]),
    [{1, #'OkeyGameR'{player_id = 2}},
     {1, #'OkeyGameR'{player_id = 1}},
     {2, #'OkeyGameR'{player_id = 3}},
     {3, #'OkeyGameR'{player_id = 4}}]
        = positions_arrange([#'OkeyGameR'{score = 40, player_id = 1},
                          #'OkeyGameR'{score = 40, player_id = 2},
                          #'OkeyGameR'{score = 20, player_id = 3},
                          #'OkeyGameR'{score = 10, player_id = 4}]),
    [{1, #'OkeyGameR'{player_id = 4}},
     {1, #'OkeyGameR'{player_id = 3}},
     {1, #'OkeyGameR'{player_id = 2}},
     {1, #'OkeyGameR'{player_id = 1}}]
        = positions_arrange([#'OkeyGameR'{score = 10, player_id = 1},
                          #'OkeyGameR'{score = 10, player_id = 2},
                          #'OkeyGameR'{score = 10, player_id = 3},
                          #'OkeyGameR'{score = 10, player_id = 4}]),

    [{1, #'OkeyGameR'{player_id = 1}},
     {2, #'OkeyGameR'{player_id = 2}},
     {3, #'OkeyGameR'{player_id = 3}},
     {4, #'OkeyGameR'{player_id = 4}}]
        = positions_arrange([#'OkeyGameR'{score = 30, player_id = 2},
                          #'OkeyGameR'{score = 20, player_id = 3},
                          #'OkeyGameR'{score = 40, player_id = 1},
                          #'OkeyGameR'{score = 10, player_id = 4}]).

score_ach_test() ->
    %% ok = app_util:start(kakaconfig),
    %% {ok, Srv} = ?MODULE:start_link([{game_mode, standard}, {sets, 2}, {rounds, 2}]),
    %% Players = [<<"gleber">>, <<"paul">>, <<"kunthar">>, <<"radistao">>],
    %% Events = [gosterge, reveal, okey, even, odd, no_reveal, with8tashes, color, wrong_reveal, caught_bluff, box_empty, out_of_tashes],
    %% lists:map(fun(UId) ->
    %%                   Event = started,
    %%                   ?MODULE:add_event(Srv, UId, Event)
    %%           end, Players),
    %% lists:map(fun(_) ->
    %%                   {UId, _} = kakamath:draw_random(Players),
    %%                   {Event, _} = kakamath:draw_random(Events),
    %%                   ?MODULE:add_event(Srv, UId, Event)
    %%           end, lists:seq(1, crypto:rand_uniform(10, 30))),
    %% {Gosterge, Hands0, _Pile0} = game_okey:hand_out_pieces(),
    %% Hands = [ {UId, put_some_nulls(21, H)} || {UId, H} <- lists:zip(Players, Hands0) ],
    %% ?MODULE:finish_round(Srv, [<<"paul">>], Hands, Gosterge).
    ok.

put_some_nulls(Target, List) ->
    lists:foldl(fun(_, Acc) ->
                        insert_at_random(null, Acc)
                end, List, lists:seq(length(List), Target)).

insert_at_random(Element, List) ->
    L1 = length(List)+1,
    R = crypto:rand_uniform(0, L1),
    case R of
        0 -> [Element | List];
        L1 -> List ++ [Element];
        _ -> lists:sublist(List, R) ++ [Element] ++ lists:nthtail(R, List)
    end.



        % [gosterge],                  %% 1
        % [reveal],                    %% 2
        % [reveal, okey],              %% 3
        % [reveal, even],              %% 4
        % [reveal, even, okey],        %% 5
        % [no_reveal, with8tashes],    %% 6
        % [reveal, color],             %% 7
        % [reveal, color, even],       %% 8
        % [reveal, color, okey],       %% 9
        % [reveal, color, even, okey], %% 10
        % [wrong_reveal],              %% 11
        % [caught_bluff],              %% 12 %% AKA others_on_wrong_reveal
        % [box_empty]                  %% 13


