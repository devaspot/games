%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description :
%%%
%%% Created : Jan 21, 2013
%%% -------------------------------------------------------------------
-module(tavla_table).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("server/include/basic_types.hrl").
-include_lib("server/include/settings.hrl").
-include_lib("server/include/game_tavla.hrl").
-include_lib("server/include/game_state.hrl").
-include_lib("server/include/requests.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/3,
         player_action/3,
         parent_message/2,
         relay_message/2
        ]).

-export([submit/3, signal/3]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-type color() :: black | white.

-record(desk_state,
        {
         state              :: state_playing | state_finished,
         board              :: term(),
         cur_color          :: integer(),
         dice               :: {undefined | integer(), undefined | integer()}, %% The dice are used for first move order
                               %% determination and for playing. In first case the first element is for white die, and the
                               %% second one for black die.
         pips_list          :: list(integer()),
         finish_reason      :: undefined | win | round_timeout | set_timeout | surrender,
         finish_info        :: undefined | {black | white, normal | mars}
        }).


-record(player,{?PLAYER, color :: undefined | color()}).

-define(STATE_WAITING_FOR_START, state_waiting_for_start).
-define(STATE_FIRST_MOVE_COMPETITION, state_first_move_competition).
-define(STATE_PLAYING, state_playing).
-define(STATE_FINISHED, state_finished).
-define(STATE_PAUSE, state_pause).
-define(STATE_SET_FINISHED, state_set_finished).

-define(RELAY, relay).
-define(DESK, tavla_desk).
-define(SCORING, tavla_scoring).
-define(LIB, tavla_lib).

-define(BLACK, black).
-define(WHITE, white).
-define(WHITE_OUT, wo).
-define(WHITE_BAR, wb).
-define(BLACK_OUT, bo).
-define(BLACK_BAR, bb).

%% ====================================================================
%% External functions
%% ====================================================================

start(GameId, TableId, TableParams) ->
    gen_fsm:start(?MODULE, [GameId, TableId, TableParams], []).

player_action(Srv, PlayerId, Action) ->
    gen_fsm:sync_send_all_state_event(Srv, {player_action, PlayerId, Action}).

parent_message(Srv, Message) ->
    gen_fsm:send_all_state_event(Srv, {parent_message, Message}).

relay_message(Srv, Message) ->
    gen_fsm:send_all_state_event(Srv, {relay_message, Message}).


submit(Table, PlayerId, Action) ->
    player_action(Table, PlayerId, {submit, Action}).

signal(Table, PlayerId, Signal) ->
    player_action(Table, PlayerId, {signal, Signal}).

%% ====================================================================
%% Server functions
%% ====================================================================
init([GameId, TableId, Params]) ->
    Parent = proplists:get_value(parent, Params),
    PlayersInfo = proplists:get_value(players, Params),
    TableName = proplists:get_value(table_name, Params),
    MultFactor = proplists:get_value(mult_factor, Params),
    SlangFlag = proplists:get_value(slang_allowed, Params),
    ObserversFlag = proplists:get_value(observers_allowed, Params),
    TournamentType = proplists:get_value(tournament_type, Params),
    Speed = proplists:get_value(speed, Params),
    TurnTimeout = proplists:get_value(turn_timeout, Params, get_timeout(turn, Speed)), %% TODO Set this param explictly
    ReadyTimeout = proplists:get_value(ready_timeout, Params, get_timeout(ready, Speed)), %% TODO Set this param explictly
    RoundTimeout = proplists:get_value(round_timeout, Params),
    SetTimeout = proplists:get_value(set_timeout, Params),
    GameMode = proplists:get_value(game_mode, Params),
    Rounds = proplists:get_value(rounds, Params),
    NextSeriesConfirmation = proplists:get_value(next_series_confirmation, Params),
    PauseMode = proplists:get_value(pause_mode, Params),
    SocialActionsEnabled = proplists:get_value(social_actions_enabled, Params),
    TTable = proplists:get_value(ttable, Params),
    Tour = proplists:get_value(tour, Params),
    Tours = proplists:get_value(tours, Params),
    TablesNum = proplists:get_value(tables_num, Params),
    %% Next two options will be passed on table respawn (after fail or service maintaince)
    ScoringState = proplists:get_value(scoring_state, Params, init_scoring(GameMode, PlayersInfo, Rounds)),
    CurRound = proplists:get_value(cur_round, Params, 0),

    Players = init_players(PlayersInfo),
    RelayParams = [{players, [{PlayerId, UserInfo#'PlayerInfo'.id} || {PlayerId, UserInfo, _, _} <- PlayersInfo]},
                   {observers_allowed, false},
                   {table, {?MODULE, self()}}],
    {ok, Relay} = ?RELAY:start(RelayParams),
    [gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Parameter <~p> : ~p", [GameId, TableId, P, V]) ||
           {P, V} <- Params],
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Started.", [GameId, TableId]),
    parent_notify_table_created(Parent, TableId, Relay),
    {_, ParentPid} = Parent,
    ParentMonRef = erlang:monitor(process, ParentPid),
    {ok, ?STATE_WAITING_FOR_START, #tavla_state{game_id = GameId,
                                          table_id = TableId,
                                          table_name = TableName,
                                          parent = Parent,
                                          parent_mon_ref = ParentMonRef,
                                          relay = Relay,
                                          mult_factor = MultFactor,
                                          slang_flag = SlangFlag,
                                          observer_flag = ObserversFlag,
                                          tournament_type = TournamentType,
                                          tour = Tour,
                                          tours = Tours,
                                          speed = Speed,
                                          turn_timeout = TurnTimeout,
                                          ready_timeout = ReadyTimeout,
                                          round_timeout = RoundTimeout,
                                          set_timeout = SetTimeout,
                                          game_mode = GameMode,
                                          rounds = Rounds,
                                          next_series_confirmation = NextSeriesConfirmation,
                                          pause_mode = PauseMode,
                                          social_actions_enabled = SocialActionsEnabled,
                                          players = Players,
                                          start_color = undefined,
                                          cur_round = CurRound,
                                          scoring_state = ScoringState,
                                          tournament_table = TTable,
                                          tables_num = TablesNum
                                         }}.

handle_event({parent_message, Message}, StateName,
             #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Received message from the parent: ~p.",
          [GameId, TableId, Message]),
    handle_parent_message(Message, StateName, StateData);

handle_event({relay_message, Message}, StateName,
             #tavla_state{game_id = GameId, table_id = TableId} =  StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Received message from the relay: ~p.",
          [GameId, TableId, Message]),
    handle_relay_message(Message, StateName, StateData);

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({player_action, PlayerId, Action}, From, StateName,
                  #tavla_state{players = Players} = StateData) ->
    case get_player(PlayerId, Players) of
        {ok, Player} ->
            handle_player_action(Player, Action, From, StateName, StateData);
        error ->
            {reply, {error, you_are_not_a_player}, StateName, StateData}
    end;

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

handle_info({timeout, Magic}, ?STATE_FIRST_MOVE_COMPETITION,
            #tavla_state{timeout_magic = Magic, game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> First move competition timeout. Do an automatic rolls.", [GameId, TableId]),
    do_first_move_competition_timeout_rolls(StateData);

handle_info({timeout, Magic}, ?STATE_PLAYING,
            #tavla_state{timeout_magic = Magic, game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Move timeout. Do an automatic action(s).", [GameId, TableId]),
    do_timeout_moves(StateData);

handle_info({round_timeout, Round}, ?STATE_PLAYING,
            #tavla_state{cur_round = Round, desk_state = DeskState, game_id = GameId,
                   table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Time to finish round ~p because of the "
          "round timeout.", [GameId, TableId, Round]),
    finalize_round(StateData#tavla_state{desk_state = DeskState#desk_state{finish_reason = timeout}});

handle_info(set_timeout, ?STATE_PLAYING,
            #tavla_state{cur_round = Round, desk_state = DeskState, game_id = GameId,
                   table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Time to finish round ~p and the set because of "
          "the set timeout.", [GameId, TableId, Round]),
    finalize_round(StateData#tavla_state{desk_state = DeskState#desk_state{finish_reason = set_timeout}});

handle_info(set_timeout, ?STATE_SET_FINISHED = StateName,
            #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Time to finish the set because of the set timeout. "
          "But the set is finished already. Ignoring", [GameId, TableId]),
    {next_state, StateName, StateData};

handle_info(set_timeout, StateName,
            #tavla_state{game_id = GameId, table_id = TableId, scoring_state = ScoringState,
                   parent = Parent, players = Players, timeout_timer = TRef} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Time to finish the set because of the set "
          "timeout at state <~p>. The set is over", [GameId, TableId, StateName]),
    if TRef =/= undefined -> erlang:cancel_timer(TRef);
       true -> do_nothing end,
    NewStateData = StateData#tavla_state{timeout_timer = undefined,
                                   timeout_magic = undefined},
    {_, RoundScore, _, TotalScore} = ?SCORING:last_round_result(ScoringState),
    RoundScorePl = [{get_player_id_by_seat_num(SeatNum, Players), Points} || {SeatNum, Points} <- RoundScore],
    TotalScorePl = [{get_player_id_by_seat_num(SeatNum, Players), Points} || {SeatNum, Points} <- TotalScore],
    parent_send_set_res(Parent, TableId, ScoringState, RoundScorePl, TotalScorePl),
    {next_state, ?STATE_SET_FINISHED, NewStateData};

handle_info({'DOWN', MonitorRef, _Type, _Object, Info}, _StateName,
             #tavla_state{game_id = GameId, table_id = TableId, parent_mon_ref = MonitorRef
                   } = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> The parent is died with reason: ~p. Stopping",
          [GameId, TableId, Info]),
    {stop, parent_died, StateData};

handle_info(Info, StateName, #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Unexpected message(info) received at state <~p>: ~p.",
          [GameId, TableId, StateName, Info]),
    {next_state, StateName, StateData}.

terminate(Reason, StateName, #tavla_state{game_id = GameId, table_id = TableId, relay = Relay}) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Shutting down at state: <~p>. Reason: ~p",
          [GameId, TableId, StateName, Reason]),
    relay_stop(Relay),
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

%% handle_parent_message(Msg, StateName, StateData)

handle_parent_message({register_player, RequestId, UserInfo, PlayerId, SeatNum}, StateName,
                      #tavla_state{table_id = TableId, players = Players,
                             parent = Parent, relay = Relay} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    Color = if SeatNum == 1 -> ?WHITE;
               SeatNum == 2 -> ?BLACK end,
    NewPlayers = reg_player(PlayerId, SeatNum, Color, UserId, IsBot, UserInfo, _Connected = false, Players),
    relay_register_player(Relay, UserId, PlayerId),
    %% TODO: Send notificitations to gamesessions (we have no such notification)
    parent_confirm_registration(Parent, TableId, RequestId),
    {next_state, StateName, StateData#tavla_state{players = NewPlayers}};

handle_parent_message({replace_player, RequestId, UserInfo, PlayerId, SeatNum}, StateName,
                      #tavla_state{table_id = TableId, players = Players,
                             parent = Parent, relay = Relay} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    #player{id = OldPlayerId} = get_player_by_seat_num(SeatNum, Players),
    Color = if SeatNum == 1 -> ?WHITE;
               SeatNum == 2 -> ?BLACK end,
    NewPlayers = del_player(OldPlayerId, Players),
    NewPlayers2 = reg_player(PlayerId, SeatNum, Color, UserId, IsBot, UserInfo, _Connected = false, NewPlayers),
    relay_kick_player(Relay, OldPlayerId),
    relay_register_player(Relay, UserId, PlayerId),
    ReplaceMsg = create_player_left(SeatNum, UserInfo, Players),
    publish_ge(ReplaceMsg, StateData),
    parent_confirm_replacement(Parent, TableId, RequestId),
    {next_state, StateName, StateData#tavla_state{players = NewPlayers2}};

handle_parent_message(start_round, ?STATE_WAITING_FOR_START,
                      #tavla_state{game_id = GameId, table_id = TableId, turn_timeout = TurnTimeout, game_mode = GameMode,
                             round_timeout = RoundTimeout, set_timeout = SetTimeout} = StateData) ->
    CurRound = 1,
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Recieved the directive to start new round (~p)", [GameId, TableId, CurRound]),
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> The start color will be determined by the competition rolls", [GameId, TableId]),
    %% This fake desk state is needed because the order of the first move is not defined yet, so
    %% we can't use the desk module to create it.
    DeskState = #desk_state{state = undefined,
                            board = init_board(),
                            dice = {undefined, undefined},
                            cur_color = undefined,
                            finish_reason = undefined,
                            finish_info = undefined},
    %% Init timers
    {TRef, Magic} = start_timer(TurnTimeout),
    RoundTRef = if is_integer(RoundTimeout) ->
                       erlang:send_after(RoundTimeout, self(), {round_timeout, CurRound});
                   true -> undefined
                end,
    SetTRef = if is_integer(SetTimeout) -> erlang:send_after(SetTimeout, self(), set_timeout);
                 true -> undefined
              end,
    NewStateData = StateData#tavla_state{desk_state = DeskState,
                                   cur_round = CurRound,
                                   timeout_timer = TRef,
                                   timeout_magic = Magic,
                                   round_timer = RoundTRef,
                                   set_timer = SetTRef},
    DoCompetitionRoll = if GameMode == paired -> false; true -> true end,
    GameStartedMsg = create_tavla_game_started(DeskState, DoCompetitionRoll, NewStateData),
    publish_ge(GameStartedMsg, NewStateData),
    {next_state, ?STATE_FIRST_MOVE_COMPETITION, NewStateData};

handle_parent_message(start_round, ?STATE_FINISHED,
                      #tavla_state{game_id = GameId, table_id = TableId, cur_round = CurRound,
                             start_color = LastStartColor, turn_timeout = TurnTimeout,
                             round_timeout = RoundTimeout, set_timeout = SetTimeout,
                             set_timer = SetTRef, game_mode = GameMode} = StateData) ->
    NewCurRound = CurRound + 1,
    StartColor = opponent_color(LastStartColor),
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Recieved the directive to start new round (~p)", [GameId, TableId, NewCurRound]),
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Start color is ~p", [GameId, TableId, StartColor]),
    Params = [{home_hit_and_run, enabled},
              {bearoff_waste_moves, enabled},
              {first_move, StartColor}],
    {ok, Desk} = ?DESK:start(Params),
    DeskState = #desk_state{state = state_wait_roll,
                            board = init_board(),
                            dice = {undefined, undefined},
                            cur_color = StartColor,
                            finish_reason = undefined,
                            finish_info = undefined},
%%    DeskState = init_desk_state(Desk),
    %% Init timers
    {TRef, Magic} = start_timer(TurnTimeout),
    RoundTRef = if is_integer(RoundTimeout) ->
                       erlang:send_after(RoundTimeout, self(), {round_timeout, NewCurRound});
                   true -> undefined
                end,
    NewSetTRef = if NewCurRound == 1 ->
                        if is_integer(SetTimeout) -> erlang:send_after(SetTimeout, self(), set_timeout);
                           true -> undefined
                        end;
                    true -> SetTRef
                 end,
    NewStateData = StateData#tavla_state{cur_round = NewCurRound,
                                   start_color = StartColor,
                                   desk_rule_pid = Desk,
                                   desk_state = DeskState,
                                   timeout_timer = TRef,
                                   timeout_magic = Magic,
                                   round_timer = RoundTRef,
                                   set_timer = NewSetTRef},
    %% Send notifications to clients
    GameStartedMsg = create_tavla_game_started(DeskState, _DoRollMove = false, NewStateData),
    publish_ge(GameStartedMsg, NewStateData),
    if GameMode =/= paired ->
           CurColor = DeskState#desk_state.cur_color,
           publish_ge(create_tavla_next_turn(CurColor, NewStateData), NewStateData);
       true -> do_nothing
    end,
    {next_state, ?STATE_PLAYING, NewStateData};

handle_parent_message(show_round_result, StateName,
                      #tavla_state{scoring_state = ScoringState,
                             game_id = GameId, table_id = TableId} = StateData) ->
    {FinishInfo, RoundScore, AchsPoints, TotalScore} = ?SCORING:last_round_result(ScoringState),
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> RoundScore: ~p Total score: ~p.", [GameId, TableId, RoundScore, TotalScore]),
    Msg = case FinishInfo of
              {win, Winner, _Condition} ->
                  create_tavla_round_ended_win(Winner, RoundScore, TotalScore, AchsPoints,
                                               StateData);
              {surrender, Surrender, _Condition} ->
                  create_tavla_round_ended_surrender(Surrender, RoundScore, TotalScore, AchsPoints,
                                                     StateData);
              timeout ->
                  create_tavla_round_ended_draw(round_timeout, RoundScore, TotalScore, AchsPoints,
                                                StateData);
              set_timeout ->
                  create_tavla_round_ended_draw(set_timeout, RoundScore, TotalScore, AchsPoints,
                                                StateData)
          end,
    publish_ge(Msg, StateData),
    {next_state, StateName, StateData#tavla_state{}};

%% Results = [{PlayerId, Position, Score, Status}] Status = winner | loser | eliminated | none
handle_parent_message({show_series_result, Results}, StateName,
                      StateData) ->
    Msg = create_tavla_series_ended(Results, StateData),
    publish_ge(Msg, StateData),
    {next_state, StateName, StateData#tavla_state{}};

%% Results = [{UserId, Position, Score, Status}] Status = active | eliminated
handle_parent_message({tour_result, TourNum, Results}, StateName,
                      #tavla_state{tournament_table = TTable} = StateData) ->
    NewTTable = [{TourNum, Results} | TTable],
    Msg = create_tavla_tour_result(TourNum, Results, StateData),
    publish_ge(Msg, StateData),
    {next_state, StateName, StateData#tavla_state{tournament_table = NewTTable}};

handle_parent_message({playing_tables_num, Num}, StateName,
                      StateData) ->
%%XXX The request for the feature was canncelled
%%    Msg = create_tavla_playing_tables(Num),
%%    publish_ge(Msg, StateData),
    {next_state, StateName, StateData};

handle_parent_message(rejoin_players, StateName,
                      #tavla_state{game_id = GameId, relay = Relay,
                             players = Players} = StateData) ->
    [relay_unregister_player(Relay, P#player.id, {rejoin, GameId}) || P <- players_to_list(Players)],
    {next_state, StateName, StateData#tavla_state{players = players_init()}};


handle_parent_message(disconnect_players, StateName,
                      #tavla_state{relay = Relay, players = Players} = StateData) ->
    [relay_unregister_player(Relay, P#player.id, game_over) || P <- players_to_list(Players)],
    {next_state, StateName, StateData#tavla_state{players = players_init()}};

handle_parent_message({send_table_state, DestTableId, PlayerId, Ref}, StateName,
                      #tavla_state{game_id = GameId, table_id = TableId, parent = Parent} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p>  Received request to send the table state events for player "
          "<~p> (~p) at table ~p. Processing.",
          [GameId, TableId, PlayerId, Ref, DestTableId]),
    GI = create_tavla_game_info(StateData),
    PlState = create_tavla_game_player_state(PlayerId, StateName, StateData),
    parent_table_state_to_player(Parent, TableId, DestTableId, PlayerId, Ref, GI),
    parent_table_state_to_player(Parent, TableId, DestTableId, PlayerId, Ref, PlState),
    {next_state, StateName, StateData};

handle_parent_message(stop, _StateName, StateData) ->
    {stop, normal, StateData};


handle_parent_message({action, {competition_rolls, Die1, Die2} = Action},
                      ?STATE_FIRST_MOVE_COMPETITION = StateName,
                      #tavla_state{game_id = GameId, table_id = TableId,
                             desk_state = DeskState} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p>  Parent action received in state <~p>: ~p. Processing.",
          [GameId, TableId, StateName, Action]),
%%    publish_ge(create_tavla_rolls_die(?WHITE, Die1, StateData), StateData),
%%    publish_ge(create_tavla_rolls_die(?BLACK, Die2, StateData), StateData),
    NewDeskState = DeskState#desk_state{dice = {Die1, Die2}},
    do_start_game(StateData#tavla_state{desk_state = NewDeskState});

handle_parent_message({action, {rolls, Color, Die1, Die2} = Action},
                      ?STATE_PLAYING = StateName,
                      #tavla_state{game_id = GameId, table_id = TableId,
                             relay = Relay, turn_timeout = TurnTimeout} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p>  Parent action received in state <~p>: ~p. Processing.",
          [GameId, TableId, StateName, Action]),
    relay_publish_ge(Relay, create_tavla_next_turn(Color, StateData)),
    {TRef, Magic} = start_timer(TurnTimeout),
    NewStateData1 = StateData#tavla_state{timeout_timer = TRef, timeout_magic = Magic},
    case do_parent_game_action(Color, {roll, Die1, Die2}, NewStateData1) of
        {ok, NewStateName, NewStateData2} ->
            {next_state, NewStateName, NewStateData2};
        {error, Reason} ->
            {stop, {parent_action_failed, Reason}, NewStateData1}
    end;

handle_parent_message({action, Action}, StateName,
                      #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p>  Parent action received in state <~p>: ~p. Ignoring.",
          [GameId, TableId, StateName, Action]),
    {next_state, StateName, StateData};

handle_parent_message({game_event, GameEvent}, StateName,
                      #tavla_state{game_id = GameId, table_id = TableId, relay = Relay} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p>  A game event received from the parent in state <~p>: ~p. Publish it.",
          [GameId, TableId, StateName, GameEvent]),
    relay_publish_ge(Relay, GameEvent),
    {next_state, StateName, StateData};

handle_parent_message({table_state_event, _PlayerId, SubscrId, StateEvent}, StateName,
                      #tavla_state{game_id = GameId, table_id = TableId, relay = Relay} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p>  A table state event received from the parent in state <~p>: ~p.",
          [GameId, TableId, StateName, StateEvent]),
    send_to_subscriber_ge(Relay, SubscrId, StateEvent),
    {next_state, StateName, StateData};

handle_parent_message(Message, StateName,
                      #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:error(?MODULE,"TAVLA_NG_TABLE <~p,~p> Unexpected parent message received in state <~p>: ~p. Stopping.",
           [GameId, TableId, StateName, Message]),
    {stop, unexpected_parent_message, StateData}.


%%===================================================================

%% handle_relay_message(Msg, StateName, StateData)

handle_relay_message({player_connected, PlayerId} = Msg, StateName,
                     #tavla_state{parent = Parent, game_id = GameId,
                            table_id = TableId, players = Players} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Received nofitication from the relay: ~p", [GameId, TableId, Msg]),
    case get_player(PlayerId, Players) of
        {ok, Player} ->
            NewPlayers = store_player_rec(Player#player{connected = true}, Players),
            parent_send_player_connected(Parent, TableId, PlayerId),
            {next_state, StateName, StateData#tavla_state{players = NewPlayers}};
        error ->
            {next_state, StateName, StateData}
    end;


handle_relay_message({player_disconnected, PlayerId}, StateName,
                     #tavla_state{parent = Parent, table_id = TableId, players = Players} = StateData) ->
    case get_player(PlayerId, Players) of
        {ok, Player} ->
            NewPlayers = store_player_rec(Player#player{connected = false}, Players),
            parent_send_player_disconnected(Parent, TableId, PlayerId),
            {next_state, StateName, StateData#tavla_state{players = NewPlayers}};
        error ->
            {next_state, StateName, StateData}
    end;

handle_relay_message({subscriber_added, PlayerId, SubscrId} = Msg, StateName,
                     #tavla_state{relay = Relay, game_id = GameId, game_mode = GameMode,
                            table_id = TableId, tournament_table = TTable,
                            players = Players, parent = Parent} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Received nofitication from the relay: ~p", [GameId, TableId, Msg]),
    IsValidPlayerId = case PlayerId of
                          observer -> true;
                          administrator -> true;
                          _ ->
                              case get_player(PlayerId, Players) of
                                  {ok, _} -> true;
                                  error -> false
                              end
                      end,
    if IsValidPlayerId ->
           GI = create_tavla_game_info(StateData),
           PlState = create_tavla_game_player_state(PlayerId, StateName, StateData),
           send_to_subscriber_ge(Relay, SubscrId, GI),
           send_to_subscriber_ge(Relay, SubscrId, PlState),
           relay_allow_broadcast_for_player(Relay, PlayerId),
           if TTable =/= undefined ->
                  [send_to_subscriber_ge(Relay, SubscrId, create_tavla_tour_result(TurnNum, Results, StateData))
                     || {TurnNum, Results} <- lists:sort(TTable)];
              true -> do_nothing
           end,
           if GameMode == paired ->
                  parent_send_get_tables_states(Parent, TableId, PlayerId, SubscrId);
              true -> do_nothing
           end;
       true -> do_nothing
    end,
    {next_state, StateName, StateData};

handle_relay_message(Message, StateName, #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:error(?MODULE,"TAVLA_NG_TABLE <~p,~p> Unknown relay message received in state <~p>: ~p. State: ~p. Stopping.",
           [GameId, TableId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================

%% handle_player_action(Player, Msg, StateName, StateData)

handle_player_action(#player{id = PlayerId, seat_num = SeatNum, user_id = UserId},
                     {submit, #game_action{action = Action, args = Args} = GA}, From,
                     StateName,
                     #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    {Keys,Values} = lists:unzip(Args),
    ExtAction = list_to_tuple([Action|Values]),
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Player <~p> (~p) submit the game action: ~p.",
        [GameId, TableId, PlayerId, UserId, ExtAction]),
    do_action(SeatNum, ExtAction, From, StateName, StateData);

handle_player_action(#player{id = PlayerId, user_id = UserId},
                     {signal, {pause_game, _}=Signal}, _From,
                     StateName,
                     #tavla_state{table_id = TableId, game_id = GameId, timeout_timer = TRef,
                            pause_mode = PauseMode, relay = Relay} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Received signal from player <~p> : ~p. PauseMode: ~p",
          [GameId, TableId, PlayerId, Signal, PauseMode]),
    case PauseMode of
        disabled ->
            {reply, {error, pause_disabled}, StateName, StateData};
        normal ->
            if StateName == ?STATE_PLAYING ->
                   Timeout = case erlang:cancel_timer(TRef) of
                                 false -> 0;
                                 T -> T
                             end,
                   relay_publish(Relay, create_game_paused_pause(UserId, GameId)),
                   {reply, 0, ?STATE_PAUSE, StateData#tavla_state{paused_statename = StateName,
                                                            paused_timeout_value = Timeout,
                                                            timeout_magic = undefined}};
               true ->
                   {reply, {error, pause_not_possible}, StateName, StateData}
            end
    end;


handle_player_action(#player{id = PlayerId, user_id = UserId},
                     {signal, {resume_game, _}=Signal}, _From,
                     StateName,
                     #tavla_state{table_id = TableId, game_id = GameId, pause_mode = PauseMode,
                            relay = Relay, paused_statename = ResumedStateName,
                            paused_timeout_value = Timeout} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Received signal from player <~p> : ~p. PauseMode: ~p",
          [GameId, TableId, PlayerId, Signal, PauseMode]),
    case PauseMode of
        disabled ->
            {reply, {error, pause_disabled}, StateName, StateData};
        normal ->
            if StateName == ?STATE_PAUSE ->
                   relay_publish(Relay, create_game_paused_resume(UserId, GameId)),
                   {TRef, Magic} = start_timer(Timeout),
                   {reply, 0, ResumedStateName, StateData#tavla_state{timeout_timer = TRef,
                                                                timeout_magic = Magic}};
               true ->
                   {reply, {error, game_is_not_paused}, StateName, StateData}
            end
    end;


handle_player_action(#player{id = PlayerId},
                     {signal, Signal}, _From, StateName,
                     #tavla_state{table_id = TableId, game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Received signal from player <~p> : ~p. Ignoring.",
          [GameId, TableId, PlayerId, Signal]),
    {reply, ok, StateName, StateData};


handle_player_action(_Player, _Message, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.



%%===================================================================

do_action(SeatNum, #tavla_roll{}, From, ?STATE_FIRST_MOVE_COMPETITION = StateName,
          #tavla_state{game_id = GameId, table_id = TableId,
                 desk_state = DeskState, players = Players} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Action tavla_roll{} Deskstate: ~p.",
          [GameId, TableId, DeskState]),
    #desk_state{dice = Dice} = DeskState,
    #player{color = Color} = get_player_by_seat_num(SeatNum, Players),
    Pos = if Color == ?WHITE -> 1;
             Color == ?BLACK -> 2 end,
    if element(Pos, Dice) == undefined ->
           Die = random_die(),
           publish_ge(create_tavla_rolls_die(Color, Die, StateData), StateData),
           NewDice = {WhiteDie, BlackDie} = setelement(Pos, Dice, Die),
           NewDeskState = DeskState#desk_state{dice = NewDice},
           if WhiteDie =/= undefined andalso BlackDie =/= undefined ->
                  gen_fsm:reply(From, ok),
                  do_start_game(StateData#tavla_state{desk_state = NewDeskState});
              true ->
                  {reply, ok, StateName, StateData#tavla_state{desk_state = NewDeskState}}
           end;
       true ->
           {reply, {error, already_rolled}, StateName, StateData}
    end;

do_action(SeatNum, #tavla_roll{}, From, ?STATE_PLAYING = StateName,
          #tavla_state{game_mode = standard,
                 desk_state = DeskState, players = Players} = StateData) ->
    #desk_state{state = DeskStateName,
                cur_color = CurColor} = DeskState,
    #player{color = Color} = get_player_by_seat_num(SeatNum, Players),
    if CurColor == Color ->
           if DeskStateName == state_wait_roll ->
                  {Die1, Die2} = random_dice(),
                  do_game_action(Color, {roll, Die1, Die2}, From, StateName, StateData);
              true ->
                  {reply, {error, already_rolled}, StateName, StateData}
           end;
       true ->
           {reply, {error, not_your_turn}, StateName, StateData}
    end;

do_action(_SeatNum, #tavla_roll{}, _From, StateName, StateData) ->
    {reply, {error, action_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #tavla_move{moves = ExtMoves}, From, ?STATE_PLAYING = StateName,
          #tavla_state{desk_state = DeskState, players = Players} = StateData) ->
    #desk_state{state = DeskStateName,
                cur_color = CurColor} = DeskState,
    #player{color = Color} = get_player_by_seat_num(SeatNum, Players),
    if Color == CurColor ->
           if DeskStateName == state_wait_move ->
                  try ext_to_moves(ExtMoves) of
                      Moves ->
                          do_game_action(Color, {move, Moves}, From, StateName, StateData)
                  catch
                      _:_ ->
                          {reply, {error, invalid_action}, StateName, StateData}
                  end;
              true ->
                  {reply, {error, roll_first}, StateName, StateData}
           end;
       true ->
           {reply, {error, not_your_turn}, StateName, StateData}
    end;

do_action(_SeatNum, #tavla_move{}, _From, StateName, StateData) ->
    {reply, {error, action_not_valid_for_a_current_state}, StateName, StateData};

do_action(_SeatNum, #tavla_ready{}, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData};

do_action(SeatNum, #tavla_surrender{}, From, ?STATE_PLAYING,
          #tavla_state{desk_state = DeskState, players = Players} = StateData) ->
    #player{color = Color} = get_player_by_seat_num(SeatNum, Players),
    #desk_state{board = Board} = DeskState,
    gen_fsm:reply(From, ok),
    Condition = surrender_condition(Color, Board),
    NewDeskState = DeskState#desk_state{finish_reason = surrender,
                                        finish_info = {Color, Condition}},
    finalize_round(StateData#tavla_state{desk_state = NewDeskState});

do_action(_SeatNum, _UnsupportedAction, _From, StateName, StateData) ->
    {reply, {error, invalid_action}, StateName, StateData}.


%%===================================================================
do_first_move_competition_timeout_rolls(#tavla_state{desk_state = DeskState} = StateData) ->
    {WhiteDie, BlackDie} = DeskState#desk_state.dice,
    FinWhiteDie = if WhiteDie == undefined ->
                         NewWhiteDie = random_die(),
                         publish_ge(create_tavla_rolls_die(?WHITE, NewWhiteDie, StateData), StateData),
                         NewWhiteDie;
                     true -> WhiteDie
                  end,
    FinBlackDie = if BlackDie == undefined ->
                         NewBlackDie = random_die(),
                         publish_ge(create_tavla_rolls_die(?BLACK, NewBlackDie, StateData), StateData),
                         NewBlackDie;
                     true -> BlackDie
                  end,
    NewDeskState = DeskState#desk_state{dice = {FinWhiteDie, FinBlackDie}},
    do_start_game(StateData#tavla_state{desk_state = NewDeskState}).

do_start_game(#tavla_state{desk_state = DeskState, turn_timeout = TurnTimeout} = StateData) ->
    {WhiteDie, BlackDie} = Dice = DeskState#desk_state.dice,
    StartColor = if WhiteDie >= BlackDie -> ?WHITE;
                    true -> ?BLACK
                 end,
    Params = [{home_hit_and_run, enabled},
              {bearoff_waste_moves, enabled},
              {first_move, StartColor},
              {dice, Dice}],
    {ok, Desk} = ?DESK:start(Params),
    NewDeskState = #desk_state{state = state_wait_move,
                               board = init_board(),
                               dice = {undefined, undefined},
                               pips_list = pips_list(WhiteDie, BlackDie),
                               cur_color = StartColor,
                               finish_reason = undefined,
                               finish_info = undefined},

%%    NewDeskState = init_desk_state(Desk),
    NewDeskState2 = NewDeskState#desk_state{dice = Dice},
    Msg = create_won_first_move(StartColor, Dice, _Reroll = false, StateData),
    publish_ge(Msg, StateData),
    {TRef, Magic} = start_timer(TurnTimeout),
    {next_state, ?STATE_PLAYING, StateData#tavla_state{start_color = StartColor,
                                                 desk_rule_pid = Desk,
                                                 desk_state = NewDeskState2,
                                                 timeout_timer = TRef,
                                                 timeout_magic = Magic
                                                }}.

do_timeout_moves(#tavla_state{desk_rule_pid = Desk, desk_state = DeskState,
                        game_id = GameId, table_id = TableId,
                        players = Players, game_mode = GameMode} = StateData) ->
    #desk_state{state = DeskStateName,
                pips_list = PipsList,
                cur_color = CurColor,
                board = Board} = DeskState,
    [#player{user_id = UserId}] = find_players_by_color(CurColor, Players),
    case DeskStateName of
        state_wait_roll when GameMode == standard ->
            gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Do automatic roll for player <~p> (~p)",
                  [GameId, TableId, CurColor, UserId]),
            {Die1, Die2} = random_dice(),
            {ok, Events1} = desk_player_action(Desk, CurColor, {roll, Die1, Die2}),
            case [E || {next_player, _} = E <- Events1] of
                [] -> %% Player can move => rolls_moves_timeout
                    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Do automatic move with dice ~p for player <~p> (~p)",
                          [GameId, TableId, {Die1, Die2}, CurColor, UserId]),
                    NewPipsList = pips_list(Die1, Die2),
                    Moves = find_moves(CurColor, NewPipsList, Board),
                    {ok, Events2} = desk_player_action(Desk, CurColor, {move, Moves}),
                    Events = [case E of
                                  {moves, CurColor, M} ->
                                      {rolls_moves_timeout, CurColor, Die1, Die2, M};
                                  _ -> E
                              end || E <- Events2],
                    NewDeskState = DeskState#desk_state{pips_list = NewPipsList},
                    process_game_events(Events, StateData#tavla_state{desk_state = NewDeskState});
                _ -> %% Only rolls_timeout
                    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> No moves can be done for the dice ~p for player <~p> (~p)",
                          [GameId, TableId, {Die1, Die2}, CurColor, UserId]),
                    Events = [case E of
                                  {rolls, CurColor, D1, D2} ->
                                      {rolls_timeout, CurColor, D1, D2};
                                  _ -> E
                              end || E <- Events1],
                    process_game_events(Events, StateData)
                end;
        state_wait_move ->
            gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Do rest automatic move with pips ~p for player <~p> (~p)",
                  [GameId, TableId, PipsList, CurColor, UserId]),
            Moves = find_moves(CurColor, PipsList, Board),
            {ok, Events1} = desk_player_action(Desk, CurColor, {move, Moves}),
            Events = [case E of
                          {moves, CurColor, M} ->
                              {moves_timeout, CurColor, M};
                          _ -> E
                      end || E <- Events1],
            process_game_events(Events, StateData)
    end.

%%===================================================================

do_parent_game_action(Color, GameAction,
               #tavla_state{desk_rule_pid = Desk} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE do_parent_game_action Color: ~p  GameAction: ~p", [Color, GameAction]),
    case desk_player_action(Desk, Color, GameAction) of
        {ok, Events} ->
            {next_state, NewStateName, NewStateData} = process_game_events(Events, StateData),
            {ok, NewStateName, NewStateData};
        {error, Reason} ->
            {error, Reason}
    end.


do_game_action(Color, GameAction, From, StateName,
               #tavla_state{game_id = GameId, table_id = TableId, desk_rule_pid = Desk} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Appling the action: Color <~p>  GameAction: ~p",
          [GameId, TableId, Color, GameAction]),
    case desk_player_action(Desk, Color, GameAction) of
        {ok, Events} ->
            gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> The game action applied successfully.",
                  [GameId, TableId]),
            gen_fsm:reply(From, ok),
            process_game_events(Events, StateData);
        {error, Reason} ->
            gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> The action was rejected by the reason: ~p",
                  [GameId, TableId, Reason]),
            ExtError = desk_error_to_ext(Reason),
            {reply, ExtError, StateName, StateData}
    end.

%% process_game_events(Events, StateData) -> {next_state, StateName, NewStateData}
process_game_events(Events, #tavla_state{desk_state = DeskState, timeout_timer = OldTRef,
                                   round_timeout = RoundTimeout, round_timer = RoundTRef,
                                   turn_timeout = TurnTimeout, game_mode = GameMode} = StateData) ->
    NewDeskState = handle_desk_events(Events, DeskState, StateData), %% Track the desk and send game events to clients
    #desk_state{state = DeskStateName} = NewDeskState,
    case DeskStateName of
        state_finished ->
            if is_integer(RoundTimeout) -> erlang:cancel_timer(RoundTRef); true -> do_nothing end,
            erlang:cancel_timer(OldTRef),
            on_game_finish(StateData#tavla_state{desk_state = NewDeskState});
        _ ->
            case [E || {next_player, _} = E <- Events] of %% Find a next player event
                [] ->
                    {next_state, ?STATE_PLAYING, StateData#tavla_state{desk_state = NewDeskState}};
                [_|_] when GameMode == paired ->
                    erlang:cancel_timer(OldTRef),
                    {next_state, ?STATE_PLAYING, StateData#tavla_state{desk_state = NewDeskState,
                                                                 timeout_timer = undefined,
                                                                 timeout_magic = undefined}};
                [_|_] ->
                    erlang:cancel_timer(OldTRef),
                    {TRef, Magic} = start_timer(TurnTimeout),
                    {next_state, ?STATE_PLAYING, StateData#tavla_state{desk_state = NewDeskState,
                                                                 timeout_timer = TRef,
                                                                 timeout_magic = Magic}}
            end
    end.


on_game_finish(StateData) ->
    finalize_round(StateData).

%%===================================================================

finalize_round(#tavla_state{desk_state = #desk_state{finish_reason = FinishReason,
                                               finish_info = FinishInfo},
                      scoring_state = ScoringState, timeout_timer = TimeoutTRef,
                      round_timer = RoundTRef, parent = Parent, players = Players,
                      game_id = GameId, table_id = TableId, cur_round = CurRound} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Finalizing the round. Finish reason: ~p. Finish info: ~p.",
          [GameId, TableId, FinishReason, FinishInfo]),
    if TimeoutTRef =/= undefined -> erlang:cancel_timer(TimeoutTRef);
       true -> do_nothing
    end,
    if RoundTRef =/= undefined -> erlang:cancel_timer(RoundTRef);
       true -> do_nothing
    end,
    FR = case FinishReason of
             timeout -> timeout;
             set_timeout -> set_timeout;
             win ->
                 {WinnerColor, Condition} = FinishInfo,
                 [#player{seat_num = SeatNum}] = find_players_by_color(WinnerColor, Players),
                 {win, SeatNum, Condition};
             surrender ->
                 {SurrenderColor, Condition} = FinishInfo,
                 [#player{seat_num = SeatNum}] = find_players_by_color(SurrenderColor, Players),
                 {surrender, SeatNum, Condition}
         end,
    {NewScoringState, GameOver} = ?SCORING:round_finished(ScoringState, FR),
    NewStateData = StateData#tavla_state{scoring_state = NewScoringState,
                                   timeout_timer = undefined,
                                   timeout_magic = undefined,
                                   round_timer = undefined},

    {_, RoundScore, _, TotalScore} = ?SCORING:last_round_result(NewScoringState),
    RoundScorePl = [{get_player_id_by_seat_num(SeatNum, Players), Points} || {SeatNum, Points} <- RoundScore],
    TotalScorePl = [{get_player_id_by_seat_num(SeatNum, Players), Points} || {SeatNum, Points} <- TotalScore],

    if GameOver ->
           gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> The set is over.", [GameId, TableId]),
           parent_send_set_res(Parent, TableId, NewScoringState, RoundScorePl, TotalScorePl),
           {next_state, ?STATE_SET_FINISHED, NewStateData};
       true ->
           gas:info(?MODULE,"TAVLA_NG_TABLE <~p,~p> Round <~p> is over.", [GameId, TableId, CurRound]),
           parent_send_round_res(Parent, TableId, NewScoringState, RoundScorePl, TotalScorePl),
           {next_state, ?STATE_FINISHED, NewStateData}
    end.



%% handle_desk_events(Events, DeskState, StateData) -> NewDeskState
%% Tracks the desk state and sends events to clients
handle_desk_events([], DeskState, _StateData) ->
    DeskState;

handle_desk_events([Event | Events], DeskState,
                    #tavla_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> handle_desk_events/3 Event: ~p", [GameId, TableId, Event]),
    #desk_state{board = Board,
                pips_list = OldPipsList} = DeskState,
    NewDeskState =
        case Event of
            {rolls, Color, Die1, Die2} ->
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Color <~p> rolls(himself): ~p", [GameId, TableId, Color, {Die1, Die2}]),
                publish_ge(create_tavla_rolls_dice(Color, {Die1, Die2}, StateData), StateData),
                PipsList = pips_list(Die1, Die2),
                DeskState#desk_state{state = state_wait_move,
                                     dice = {Die1, Die2}, pips_list = PipsList};
            {moves, Color, Moves} ->
                [publish_ge(create_tavla_moves(Color, From, To, Type, Pips, StateData), StateData) ||
                   {Type, From, To, Pips} <- Moves],
                UsedPipsList = [Pips || {_Type, _From, _To, Pips} <- Moves],
                NewBoard = apply_moves(Color, Moves, Board),
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Color <~p> moves(himself): ~p", [GameId, TableId, Color, Moves]),
                show_boards(GameId, TableId, Board, NewBoard),
                DeskState#desk_state{board = NewBoard, pips_list = OldPipsList -- UsedPipsList};
            {rolls_timeout, Color, Die1, Die2} -> %% Injected event
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Color <~p> rolls(auto): ~p", [GameId, TableId, Color, {Die1, Die2}]),
                Msg = create_tavla_turn_timeout(Color, {Die1, Die2}, _Moves = [], StateData),
                publish_ge(Msg, StateData),
                DeskState#desk_state{dice = {Die1, Die2}};
            {rolls_moves_timeout, Color, Die1, Die2, Moves} -> %% Injected event
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Color <~p> rolls(auto): ~p", [GameId, TableId, Color, {Die1, Die2}]),
                Msg = create_tavla_turn_timeout(Color, {Die1, Die2}, Moves, StateData),
                publish_ge(Msg, StateData),
                NewBoard = apply_moves(Color, Moves, Board),
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Color <~p> moves(auto): ~p", [GameId, TableId, Color, Moves]),
                show_boards(GameId, TableId, Board, NewBoard),
                DeskState#desk_state{dice = {Die1, Die2}, board = NewBoard};
            {moves_timeout, Color, Moves} ->    %% Injected event
                Msg = create_tavla_turn_timeout(Color, _Dice = undefined, Moves, StateData),
                publish_ge(Msg, StateData),
                NewBoard = apply_moves(Color, Moves, Board),
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Color <~p> moves(auto): ~p", [GameId, TableId, Color, Moves]),
                show_boards(GameId, TableId, Board, NewBoard),
                DeskState#desk_state{board = NewBoard};
            {next_player, Color} ->
                gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: Waiting for Color <~p> roll...", [GameId, TableId, Color]),
                Msg = create_tavla_next_turn(Color, StateData),
                publish_ge(Msg, StateData),
                DeskState#desk_state{state = state_wait_roll, cur_color = Color,
                                     dice = {undefined, undefined},
                                     pips_list = []};
            {win, Color, Condition} ->
                DeskState#desk_state{state = state_finished,
                                     finish_reason = win,
                                     finish_info = {Color, Condition}}
        end,
    handle_desk_events(Events, NewDeskState, StateData).


show_boards(GameId, TableId, Board1, Board2) ->
    B1 = ?LIB:board_to_text4(Board1),
    B2 = ?LIB:board_to_text4(Board2),
    M = ["    ", "    ", "    ", " => " ,"    ", "    ", "    "],
    T = lists:zip3(B1, M, B2),
    [gas:info(?MODULE,"TAVLA_NG_TABLE_DBG <~p,~p> Board: ~s~s~s", [GameId, TableId, S1, S2, S3])
     || {S1, S2, S3} <- T].

%%===================================================================
init_scoring(GameType, PlayersInfo, Rounds) ->
    SeatsInfo = [{SeatNum, Points} || {_PlayerId, _UserInfo, SeatNum, Points} <- PlayersInfo],
    ?SCORING:init(GameType, SeatsInfo, Rounds).

random_die() ->
    crypto:rand_uniform(1, 7).

%% random_dice() -> {Die1, Die2}
random_dice() ->
    {random_die(), random_die()}.

%% start_timer(Timeout) -> {TRef, Magic}
start_timer(Timeout) ->
    Magic = make_ref(),
    TRef = erlang:send_after(Timeout, self(), {timeout, Magic}),
    {TRef, Magic}.

%% players_init() -> players()
players_init() ->
    midict:new().

%% reg_player(PlayerId, SeatNum, Color, UserId, IsBot, Players) -> NewPlayers
reg_player(PlayerId, SeatNum, Color, UserId, IsBot, UserInfo, Connected, Players) ->
    store_player_rec(#player{id =PlayerId, seat_num = SeatNum, color = Color, user_id = UserId,
                             is_bot = IsBot, info = UserInfo, connected = Connected}, Players).

%% reg_player(#player{}, Players) -> NewPlayers
store_player_rec(#player{id =Id, seat_num = SeatNum, color = Color, user_id = UserId,
                         is_bot = IsBot, connected = Connected} = Player, Players) ->
    Indices = [{seat_num, SeatNum}, {color, Color}, {user_id, UserId},
               {is_bot, IsBot}, {connected, Connected}],
    midict:store(Id, Player, Indices, Players).

%% get_player_id_by_seat_num(SeatNum, Players) -> PlayerId
get_player_id_by_seat_num(SeatNum, Players) ->
    [#player{id = PlayerId}] = midict:geti(SeatNum, seat_num, Players),
    PlayerId.

%% fetch_player(PlayerId, Players) -> Player
fetch_player(PlayerId, Players) ->
    midict:fetch(PlayerId, Players).

%% get_player(PlayerId, Players) -> {ok, Player} | error
get_player(PlayerId, Players) ->
    midict:find(PlayerId, Players).

%% get_player_by_seat_num(SeatNum, Players) -> Player
get_player_by_seat_num(SeatNum, Players) ->
    [Player] = midict:geti(SeatNum, seat_num, Players),
    Player.

%% find_players_by_seat_num(SeatNum, Players) -> [Player]
find_players_by_seat_num(SeatNum, Players) ->
    midict:geti(SeatNum, seat_num, Players).

find_players_by_color(Color, Players) ->
    midict:geti(Color, color, Players).


%% del_player(PlayerId, Players) -> NewPlayers
del_player(PlayerId, Players) ->
    midict:erase(PlayerId, Players).

%% players_to_list(Players) -> List
players_to_list(Players) ->
    midict:all_values(Players).

%% @spec init_players(PlayersInfo) -> Players
%% @end
%% PlayersInfo = [{PlayerId, UserInfo, SeatNum, StartPoints}]

init_players(PlayersInfo) ->
    init_players(PlayersInfo, players_init()).

init_players([], Players) ->
    Players;

init_players([{PlayerId, UserInfo, SeatNum, _StartPoints} | PlayersInfo], Players) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    Color = if SeatNum == 1 -> ?WHITE;
               SeatNum == 2 -> ?BLACK end,
    NewPlayers = reg_player(PlayerId, SeatNum, Color, UserId, IsBot, UserInfo, _Connected = false, Players),
    init_players(PlayersInfo, NewPlayers).

%%===================================================================

send_to_subscriber_ge(Relay, SubscrId, Msg) ->
    [Name|List] = tuple_to_list(Msg),
    Event = #game_event{event = Name, args = lists:zip(known_records:fields(Name),List) },
    ?RELAY:table_message(Relay, {to_subscriber, SubscrId, Event}).

relay_publish_ge(Relay, Msg) ->
    [Name|List] = tuple_to_list(Msg),
    Event = #game_event{event = Name, args = lists:zip(known_records:fields(Name),List) },
    relay_publish(Relay, Event).

relay_publish(Relay, Msg) ->
    ?RELAY:table_message(Relay, {publish, Msg}).

relay_allow_broadcast_for_player(Relay, PlayerId) ->
    ?RELAY:table_message(Relay, {allow_broadcast_for_player, PlayerId}).

relay_register_player(Relay, UserId, PlayerId) ->
    ?RELAY:table_request(Relay, {register_player, UserId, PlayerId}).

relay_unregister_player(Relay, PlayerId, Reason) ->
    ?RELAY:table_request(Relay, {unregister_player, PlayerId, Reason}).

relay_kick_player(Relay, PlayerId) ->
    ?RELAY:table_request(Relay, {kick_player, PlayerId}).

relay_stop(Relay) ->
    ?RELAY:table_message(Relay, stop).

parent_confirm_registration({ParentMod, ParentPid}, TableId, RequestId) ->
    ParentMod:table_message(ParentPid, TableId, {response, RequestId, ok}).

parent_confirm_replacement({ParentMod, ParentPid}, TableId, RequestId) ->
    ParentMod:table_message(ParentPid, TableId, {response, RequestId, ok}).

parent_notify_table_created({ParentMod, ParentPid}, TableId, RelayPid) ->
    ParentMod:table_message(ParentPid, TableId, {table_created, {?RELAY, RelayPid}}).

parent_send_round_res({ParentMod, ParentPid}, TableId, ScoringState, RoundScores, TotalScores) ->
    ParentMod:table_message(ParentPid, TableId, {round_finished, ScoringState, RoundScores, TotalScores}).

parent_send_set_res({ParentMod, ParentPid}, TableId, ScoringState, RoundScores, TotalScores) ->
    ParentMod:table_message(ParentPid, TableId, {game_finished, ScoringState, RoundScores, TotalScores}).

parent_send_player_connected({ParentMod, ParentPid}, TableId, PlayerId) ->
    ParentMod:table_message(ParentPid, TableId, {player_connected, PlayerId}).

parent_send_player_disconnected({ParentMod, ParentPid}, TableId, PlayerId) ->
    ParentMod:table_message(ParentPid, TableId, {player_disconnected, PlayerId}).

parent_send_get_tables_states({ParentMod, ParentPid}, TableId, PlayerId, SubscrId) ->
    ParentMod:table_message(ParentPid, TableId, {get_tables_states, PlayerId, SubscrId}).

parent_table_state_to_player({ParentMod, ParentPid}, TableId, DestTableId, PlayerId, Ref, StateEvent) ->
    ParentMod:table_message(ParentPid, TableId, {table_state_event, DestTableId, PlayerId, Ref, StateEvent}).

parent_publish_ge({ParentMod, ParentPid}, TableId, GameEvent) ->
    ParentMod:table_message(ParentPid, TableId, {game_event, GameEvent}).

desk_player_action(Desk, Color, Action) ->
    ?DESK:player_action(Desk, Color, Action).

publish_ge(GameEvent, #tavla_state{relay = Relay, parent = Parent, table_id = TableId,
                             game_mode = GameMode}) ->
    case GameMode of
        paired ->
            case GameEvent of
                #tavla_next_turn{} -> do_nothing;
                _ ->  relay_publish_ge(Relay, GameEvent)
            end,
            parent_publish_ge(Parent, TableId, GameEvent);
        _ ->
            relay_publish_ge(Relay, GameEvent)
    end.

%%===================================================================

create_tavla_game_info(#tavla_state{table_name = TName, mult_factor = MulFactor,
                              slang_flag = SlangFlag, observer_flag = ObserverFlag,
                              speed = Speed, turn_timeout = TurnTimeout,
                              ready_timeout = ReadyTimeout, game_mode = GameMode,
                              rounds = Rounds1, players = Players, tour = Tour,
                              tours = Tours, pause_mode = PauseMode, tables_num = TablesNum,
                              tournament_type = TournamentType, table_id = TableId,
                              social_actions_enabled = SocialActionsEnabled,
                              next_series_confirmation = ConfirmMode}) ->
    PInfos = [case find_players_by_seat_num(SeatNum, Players) of
                  [#player{info = UserInfo}] -> UserInfo;
                  [] -> null
              end || SeatNum <- [1, 2]],
    Sets = if Tours == undefined -> null; true -> Tours end,
    SetNo = if Tour == undefined -> null; true -> Tour end,
    Rounds = if Rounds1 == infinity -> -1; true -> Rounds1 end,
    #tavla_game_info{%%game_type :: atom(),
                     table_name = list_to_binary(TName),
                     game_mode = GameMode,
                     sets = Sets,
                     set_no = SetNo,
                     table_id  = TableId,
                     tables_num = TablesNum,
                     %%current_round :: integer(),
                     rounds = Rounds,
                     players = PInfos,
                     speed = Speed,
                     turn_timeout = TurnTimeout,
                     %%challenge_timeout :: integer(),   %% timeout value for challenge
                     ready_timeout = ReadyTimeout,
                     mul_factor = MulFactor,
                     slang_flag = SlangFlag,
                     observer_flag = ObserverFlag,
                     pause_enabled = PauseMode == normal,
                     social_actions_enabled = SocialActionsEnabled,
                     tournament_type = TournamentType,
                     series_confirmation_mode = list_to_binary(atom_to_list(ConfirmMode))
                    }.

create_tavla_game_player_state(_PlayerId, ?STATE_WAITING_FOR_START,
                               #tavla_state{table_id = TableId, cur_round = CurRound, players = Players,
                                      set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    Colors = players_ext_color_info(Players),
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #tavla_game_player_state{table_id = TableId,
                             board = null,
                             dice = [null, null],
                             players_colors = Colors,
                             whos_move = [],
                             game_state = initializing,
                             current_round = CurRound,
                             next_turn_in = null,
                             paused = false,
                             round_timeout = null,
                             set_timeout = SetTimeout};

create_tavla_game_player_state(_PlayerId, ?STATE_FIRST_MOVE_COMPETITION,
                               #tavla_state{timeout_timer = TRef, cur_round = CurRound,
                                      players = Players, desk_state = DeskState,
                                      round_timer = RoundTRef,
                                      round_timeout = RoundTimeout1, set_timer = SetTRef,
                                      set_timeout = SetTimeout1, table_id = TableId}) ->
    #desk_state{dice = {WhiteDie, BlackDie} = Dice,
                board = Board} = DeskState,
    Colors = players_ext_color_info(Players),
    WhosMove = case {WhiteDie, BlackDie} of
                   {undefined, undefined} -> [color_to_ext(?WHITE), color_to_ext(?BLACK)];
                   {_, undefined} -> [color_to_ext(?BLACK)];
                   {undefined, _} -> [color_to_ext(?WHITE)]
               end,
    Timeout = calc_timeout(TRef),
    RoundTimeout = if RoundTimeout1 == infinity -> null;
                      true -> calc_timeout_comp(RoundTRef, 2000)
                   end,
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #tavla_game_player_state{table_id = TableId,
                             board = board_to_ext(Board),
                             dice = dice_to_ext(Dice),
                             players_colors = Colors,
                             whos_move = WhosMove,
                             game_state = first_move_competition,
                             current_round = CurRound,
                             next_turn_in = Timeout,
                             paused = false,
                             round_timeout = RoundTimeout,
                             set_timeout = SetTimeout};


create_tavla_game_player_state(_PlayerId, ?STATE_PLAYING,
                               #tavla_state{timeout_timer = TRef, cur_round = CurRound,
                                      players = Players, desk_state = DeskState,
                                      round_timer = RoundTRef,
                                      round_timeout = RoundTimeout1, set_timer = SetTRef,
                                      set_timeout = SetTimeout1, table_id = TableId}) ->
    #desk_state{board = Board,
                cur_color = CurColor,
                dice = Dice} = DeskState,
    Colors = players_ext_color_info(Players),
    GameState = if Dice == {undefined, undefined} -> waiting_for_roll;
                   true -> waiting_for_move
                end,
    Timeout = calc_timeout(TRef),
    RoundTimeout = if RoundTimeout1 == infinity -> null;
                      true -> calc_timeout_comp(RoundTRef, 2000)
                   end,
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #tavla_game_player_state{table_id = TableId,
                             board = board_to_ext(Board),
                             dice = dice_to_ext(Dice),
                             players_colors = Colors,
                             whos_move = [color_to_ext(CurColor)],
                             game_state = GameState,
                             current_round = CurRound,
                             next_turn_in = Timeout,
                             paused = false,
                             round_timeout = RoundTimeout,
                             set_timeout = SetTimeout};

create_tavla_game_player_state(_PlayerId, ?STATE_SET_FINISHED,
                               #tavla_state{table_id = TableId, cur_round = CurRound,
                                      players = Players}) ->
    Colors = players_ext_color_info(Players),
    #tavla_game_player_state{table_id = TableId,
                             board = null,
                             dice = [null, null],
                             players_colors = Colors,
                             whos_move = [],
                             game_state = finished,
                             current_round = CurRound,
                             next_turn_in = null,
                             paused = false,
                             round_timeout = null,
                             set_timeout = null};

create_tavla_game_player_state(_PlayerId, ?STATE_FINISHED,
                               #tavla_state{table_id = TableId, cur_round = CurRound, players = Players,
                                      set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    Colors = players_ext_color_info(Players),
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #tavla_game_player_state{table_id = TableId,
                             board = null,
                             dice = [null, null],
                             players_colors = Colors,
                             whos_move = [],
                             game_state = initializing,
                             current_round = CurRound,
                             next_turn_in = null,
                             paused = false,
                             round_timeout = null,
                             set_timeout = SetTimeout};

create_tavla_game_player_state(PlayerId, ?STATE_PAUSE,
                               #tavla_state{paused_statename = PausedStateName,
                                      paused_timeout_value = Timeout
                                     } = StateData) ->
    Msg = create_tavla_game_player_state(PlayerId, PausedStateName, StateData),
    Msg#tavla_game_player_state{next_turn_in = Timeout,
                                paused = true}.


create_tavla_game_started(DeskState, DoFirstMoveCompetitionRoll,
                         #tavla_state{table_id = TableId, cur_round = CurRound,
                                round_timeout = RoundTimeout1, players = Players,
                                set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    #desk_state{board = Board} = DeskState,
    Colors = players_ext_color_info(Players),
    RoundTimeout = if RoundTimeout1 == infinity -> null;
                      true -> RoundTimeout1 - 2000
                   end,
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #tavla_game_started{table_id = TableId,
                        board = board_to_ext(Board),
                        players = Colors,
                        current_round = CurRound,
                        round_timeout = RoundTimeout,
                        set_timeout = SetTimeout,
                        do_first_move_competition_roll = DoFirstMoveCompetitionRoll}.

create_won_first_move(Color, Dice,Reroll, #tavla_state{table_id = TableId, players = Players}) ->
    [#player{user_id = UserId}] = find_players_by_color(Color, Players),
    #tavla_won_first_move{table_id = TableId,
                          color = color_to_ext(Color),
                          player = UserId,
                          dice = dice_to_ext(Dice),
                          reroll = Reroll}.

create_tavla_next_turn(Color, #tavla_state{table_id = TableId, players = Players}) ->
    [#player{user_id = UserId}] = find_players_by_color(Color, Players),
    #tavla_next_turn{table_id = TableId, color = color_to_ext(Color), player = UserId}.

create_tavla_rolls_die(Color, Die, #tavla_state{table_id = TableId, players = Players}) ->
    [#player{user_id = UserId}] = find_players_by_color(Color, Players),
    #tavla_rolls{table_id = TableId,
                 player = UserId,
                 color = color_to_ext(Color),
                 dices = [die_to_ext(Die)]
                }.

create_tavla_rolls_dice(Color, Dice, #tavla_state{table_id = TableId, players = Players}) ->
    [#player{user_id = UserId}] = find_players_by_color(Color, Players),
    #tavla_rolls{table_id = TableId,
                 player = UserId,
                 color = color_to_ext(Color),
                 dices = dice_to_ext(Dice)
                }.

create_tavla_moves(Color, From, To, Type, Pips, #tavla_state{table_id = TableId, players = Players}) ->
    [#player{user_id = UserId}] = find_players_by_color(Color, Players),
    #tavla_moves{table_id = TableId,
                 color = color_to_ext(Color),
                 player = UserId,
                 from = pos_to_ext(From),
                 to = pos_to_ext(To),
                 hits = Type == hit,
                 pips = Pips}.

create_player_left(SeatNum, UserInfo, Players) ->
    #player{user_id = OldUserId} = get_player_by_seat_num(SeatNum, Players),
    IsBot = UserInfo#'PlayerInfo'.robot,
    #player_left{player = OldUserId,
                 human_replaced = not IsBot,
                 replacement = UserInfo}.


create_tavla_round_ended_win(Winner, RoundScore, TotalScore, _PlayersAchsPoints,
                             #tavla_state{table_id = TableId, players = Players}) ->
    PlResults = [begin
                     #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                     WinnerStatus = if SeatNum == Winner -> <<"true">>;
                                       true -> <<"none">> end,
                     {_, Score} = lists:keyfind(SeatNum, 1, TotalScore),
                     {_, ScoreDelta} = lists:keyfind(SeatNum, 1, RoundScore),
                     #'TavlaPlayerScore'{player_id = UserId,
                                         %%reason :: atom(),
                                         winner = WinnerStatus,
                                         score_delta = ScoreDelta,
                                         score = Score
                                        }
                 end || SeatNum <- [1, 2]],
    Results = #'TavlaGameResults'{players = PlResults},
    #'TavlaPlayerScore'{player_id = WinnerUserId} =
                            lists:keyfind(<<"true">>, #'TavlaPlayerScore'.winner, PlResults),
    #tavla_game_ended{table_id = TableId,
                      reason = <<"win">>,
                      winner = WinnerUserId,
                      results =  Results}.

create_tavla_round_ended_surrender(Surrender, RoundScore, TotalScore, _PlayersAchsPoints,
                                   #tavla_state{table_id = TableId, players = Players}) ->
    PlResults = [begin
                     #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                     WinnerStatus = if SeatNum =/= Surrender -> <<"true">>;
                                       true -> <<"none">> end,
                     {_, Score} = lists:keyfind(SeatNum, 1, TotalScore),
                     {_, ScoreDelta} = lists:keyfind(SeatNum, 1, RoundScore),
                     #'TavlaPlayerScore'{player_id = UserId,
                                         %%reason :: atom(),
                                         winner = WinnerStatus,
                                         score_delta = ScoreDelta,
                                         score = Score
                                        }
                 end || SeatNum <- [1, 2]],
    Results = #'TavlaGameResults'{players = PlResults},
    #'TavlaPlayerScore'{player_id = WinnerUserId} =
                            lists:keyfind(<<"true">>, #'TavlaPlayerScore'.winner, PlResults),
    #tavla_game_ended{table_id = TableId,
                      reason = <<"surrender">>,
                      winner = WinnerUserId,
                      results =  Results}.

create_tavla_round_ended_draw(Reason, RoundScore, TotalScore, _PlayersAchsPoints,
                             #tavla_state{table_id = TableId, players = Players}) ->
    PlResults = [begin
                     #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                     {_, Score} = lists:keyfind(SeatNum, 1, TotalScore),
                     {_, ScoreDelta} = lists:keyfind(SeatNum, 1, RoundScore),
                     #'TavlaPlayerScore'{player_id = UserId,
                                         %%reason :: atom(),
                                         winner = <<"none">>,
                                         score_delta = ScoreDelta,
                                         score = Score
                                        }
                 end || SeatNum <- [1, 2]],
    Results = #'TavlaGameResults'{players = PlResults},
    ReasonStr = case Reason of
                    round_timeout -> <<"round_timeout">>;
                    set_timeout -> <<"set_timeout">>
                end,
    #tavla_game_ended{table_id = TableId,
                      reason = ReasonStr,
                      winner = null,
                      results =  Results}.

create_tavla_series_ended(Results, #tavla_state{table_id = TableId, players = Players}) ->
    Standings = [begin
                     #player{user_id = UserId} = fetch_player(PlayerId, Players),
                     Winner = case Status of    %% TODO: Implement in the client support of all statuses
                                  winner -> <<"true">>;
                                  _ -> <<"none">>
                              end,
                     #'TavlaSeriesResult'{player_id = UserId, place = Position, score = Score,
                                          winner = Winner}
                 end || {PlayerId, Position, Score, Status} <- Results],
    #tavla_series_ended{table_id = TableId,
                        standings = Standings}.

create_tavla_tour_result(TourNum, Results, #tavla_state{table_id = TableId}) ->
    Records = [#tavla_tour_record{player_id = UserId, place = Position,
                                  score = Score, status = Status}
               || {UserId, Position, Score, Status} <- Results],
    #tavla_tour_result{table_id = TableId,
                       tour_num = TourNum,
                       records = Records}.

create_tavla_turn_timeout(Color, Dice, Moves, #tavla_state{table_id = TableId, players = Players}) ->
    DiceExt = if Dice == undefined -> null;
                 true -> dice_to_ext(Dice) end,
    [#player{user_id = UserId}] = find_players_by_color(Color, Players),
    #tavla_turn_timeout{table_id = TableId,
                        player = UserId,
                        color = color_to_ext(Color),
                        dice = DiceExt,
                        moves = moves_to_ext(Moves)}.

create_game_paused_pause(UserId, GameId) ->
    #game_paused{game = GameId,
                 who = UserId,
                 action = <<"pause">>,
                 retries = 0}.

create_game_paused_resume(UserId, GameId) ->
    #game_paused{game = GameId,
                 who = UserId,
                 action = <<"resume">>,
                 retries = 0}.


desk_error_to_ext({position_occupied, _, _}) -> {error, position_occupied};
desk_error_to_ext({waste_move_disabled, _, _}) -> {error, waste_move_disabled};
desk_error_to_ext({hit_and_run_disabled, _, _}) -> {error, hit_and_run_disabled};
desk_error_to_ext({not_bear_off_mode, _, _}) -> {error, not_bear_off_mode};
desk_error_to_ext({no_checker, _, _}) -> {error, no_checker};
desk_error_to_ext({move_from_bar_first, _, _}) -> {error, move_from_bar_first};
desk_error_to_ext({invalid_move, _, _}) -> {error, invalid_move};
desk_error_to_ext(too_many_moves) -> {error, too_many_moves};
desk_error_to_ext(invalid_action) -> {error, invalid_action};
desk_error_to_ext(not_your_order) -> {error, not_your_turn};
desk_error_to_ext(_E) -> {error, unknown_error}.

players_ext_color_info(Players) ->
    [begin
         [#player{user_id = UId}] = find_players_by_color(C, Players),
         #tavla_color_info{name = UId, color = color_to_ext(C)}
     end || C <- [?WHITE, ?BLACK]].

board_to_ext(Board) ->
%%    gas:info(?MODULE,"board_to_ext Board: ~p", [Board]),
    Order = [?WHITE_OUT] ++ lists:seq(1, 24) ++ [?WHITE_BAR, ?BLACK_BAR, ?BLACK_OUT],
    [case lists:keyfind(Pos, 1, Board) of
         {_, empty} -> null;
         {_, {C, Num}} -> #tavla_checkers{color = color_to_ext(C), number = Num}
     end || Pos <- Order].

moves_to_ext(Moves) ->
    [#'TavlaAtomicMoveServer'{from = pos_to_ext(From),
                              to =  pos_to_ext(To),
                              hits = Type == hit,
                              pips = Pips} || {Type, From, To, Pips} <- Moves].

ext_to_moves(ExtMoves) ->
    F = fun(#'TavlaAtomicMove'{from = From, to = To}) ->
                {ext_to_pos(From), ext_to_pos(To)}
        end,
    lists:map(F, ExtMoves).

pos_to_ext(Pos) ->
    case Pos of
        ?WHITE_OUT -> 0;
        ?WHITE_BAR -> 25;
        ?BLACK_BAR -> 26;
        ?BLACK_OUT -> 27;
        X -> X
    end.

ext_to_pos(ExtPos) ->
     case ExtPos of
         0 -> ?WHITE_OUT;
         25 -> ?WHITE_BAR;
         26 -> ?BLACK_BAR;
         27 -> ?BLACK_OUT;
         X when is_integer(X), X >=1, X =< 24 -> X
     end.

%% XXX Different colors id for different external terms is strange... 
color_to_ext(?WHITE) -> 1;
color_to_ext(?BLACK) -> 2.

dice_to_ext({Die1, Die2}) -> [die_to_ext(Die1), die_to_ext(Die2)].

die_to_ext(undefined) -> null;
die_to_ext(Die) -> Die.


%%===================================================================

get_timeout(turn, fast) -> {ok, Val}   = kvs:get(config,"games/okey/turn_timeout_fast", 15000), Val;
get_timeout(turn, normal) -> {ok, Val} = kvs:get(config,"games/okey/turn_timeout_normal", 30000), Val;
get_timeout(turn, slow) -> {ok, Val}   = kvs:get(config,"games/okey/turn_timeout_slow", 60000), Val;

get_timeout(ready, fast) -> {ok, Val}   = kvs:get(config,"games/okey/ready_timeout_fast", 15000), Val;
get_timeout(ready, normal) -> {ok, Val} = kvs:get(config,"games/okey/ready_timeout_normal", 25000), Val;
get_timeout(ready, slow) -> {ok, Val}   = kvs:get(config,"games/okey/ready_timeout_slow", 45000), Val.

%%===================================================================

calc_timeout(undefined) -> 0;
calc_timeout(TRef) ->
    case erlang:read_timer(TRef) of
        false -> 0;
        Timeout -> Timeout
    end.

calc_timeout_comp(TRef, Compensation) ->
    if TRef == undefined -> null;
       true -> case erlang:read_timer(TRef) of
                   false -> 0;
                   T when T < Compensation -> 0; %% Latency time compensation
                   T -> T - Compensation
               end
    end.

%%===================================================================
init_desk_state(Desk) ->
    #desk_state{state = ?DESK:get_state_name(Desk),
                board = ?DESK:get_board(),
                cur_color = ?DESK:get_cur_color(Desk),
                dice = {undefined, undefined},
                finish_reason = undefined,
                finish_info = undefined}.

init_board() ->
    [{01, {?BLACK, 2}}, {02, empty}, {03, empty}, {04, empty}, {05, empty}, {06, {?WHITE, 5}},
     {07, empty}, {08, {?WHITE, 3}}, {09, empty}, {10, empty}, {11, empty}, {12, {?BLACK, 5}},
     {13, {?WHITE, 5}}, {14, empty}, {15, empty}, {16, empty}, {17, {?BLACK, 3}}, {18, empty},
     {19, {?BLACK, 5}}, {20, empty}, {21, empty}, {22, empty}, {23, empty}, {24, {?WHITE, 2}},
     {?WHITE_OUT, empty}, {?BLACK_OUT, empty}, {?WHITE_BAR, empty}, {?BLACK_BAR, empty}
    ].


opponent_color(?WHITE) -> ?BLACK;
opponent_color(?BLACK) -> ?WHITE.


pips_list(Die1, Die2) ->
    if Die1 == Die2 -> [Die1, Die1, Die1, Die1];
       true -> [Die1, Die2]
    end.

apply_moves(Color, Moves, Board) ->
    F = fun({Type, From, To, _Pips}, BoardAcc) ->
                apply_move(Color, From, To, Type, BoardAcc)
        end,
    lists:foldl(F, Board, Moves).

apply_move(Color, From, To, Type, Board) ->
    OppColor = opponent_color(Color),
    Board1 = case get_checkers(To, Board) of
                 empty -> set_checkers(To, {Color, 1}, Board);
                 {Color, Num} -> set_checkers(To, {Color, Num + 1}, Board);
                 {OppColor, 1} -> set_checkers(To, {Color, 1}, Board)
             end,
    Board2 = case get_checkers(From, Board1) of
                 {Color, 1} -> set_checkers(From, empty, Board1);
                 {Color, Num2} -> set_checkers(From, {Color, Num2 -1}, Board1)
             end,
    if Type == hit -> %% Increase number of the opponents battons on the bar
           BarPos = bar_position(OppColor),
           case get_checkers(BarPos, Board2) of
               empty -> set_checkers(BarPos, {OppColor, 1}, Board2);
               {OppColor, Num3} -> set_checkers(BarPos, {OppColor, Num3 + 1}, Board2)
           end;
       Type == move ->
           Board2
    end.

%% surrender_condition(Color, Board) -> normal | mars
surrender_condition(Color, Board) ->
    case get_checkers(out_position(Color), Board) of
        {Color, _} -> normal;
        empty -> mars
    end.

get_checkers(Pos, Board) ->
    {_, Value} = lists:keyfind(Pos, 1, Board),
    Value.

set_checkers(Pos, Value, Board) ->
    lists:keyreplace(Pos, 1, Board, {Pos, Value}).

out_position(?WHITE) -> ?WHITE_OUT;
out_position(?BLACK) -> ?BLACK_OUT.

bar_position(?WHITE) -> ?WHITE_BAR;
bar_position(?BLACK) -> ?BLACK_BAR.

%%===================================================================
%%

find_moves(Color, PipsList, Board) ->
    AdoptedBoard = if Color == ?BLACK -> reverse_board(Board);
                      true -> Board end,
    Moves = lists:reverse(find_moves2(AdoptedBoard, PipsList)),
    if Color == ?BLACK -> reverse_moves(Moves);
       true -> Moves
    end.

find_moves2(Board, PipsList) ->
    case find_bar_out_moves(Board, PipsList) of
        {ok, NewBoard, NewPipsList, Moves} ->
            find_normal_moves(NewBoard, NewPipsList, Moves);
        {stop, Moves} ->
            Moves
    end.

find_bar_out_moves(Board, PipsList) ->
    case get_checkers(?WHITE_BAR, Board) of
        empty -> {ok, Board, PipsList, []};
        {?WHITE, Num} -> find_bar_out_moves(Board, PipsList, PipsList, Num, [])
    end.

find_bar_out_moves(Board, OrigPipsList, _PipsList, _Num = 0, Moves) -> {ok, Board, OrigPipsList, Moves};
find_bar_out_moves(_Board, _OrigPipsList, _PipsList = [], _Num, Moves) -> {stop, Moves};
find_bar_out_moves(Board, OrigPipsList, [Pips | RestPipsList], Num, Moves) ->
    case check_move(?WHITE_BAR, Pips, Board, false) of
        {Type, To} ->
            NewBoard = apply_move(?WHITE, ?WHITE_BAR, To, Type, Board),
            find_bar_out_moves(NewBoard, OrigPipsList -- [Pips], RestPipsList, Num - 1, [{?WHITE_BAR, To} | Moves]);
        error ->
            find_bar_out_moves(Board, OrigPipsList, RestPipsList, Num, Moves)
    end.


find_normal_moves(Board, PipsList, Moves) ->
    BearOffMode = bearoff_mode(Board),
    if BearOffMode -> find_normal_moves(Board, PipsList, [], true, Moves, 6);
       true -> find_normal_moves(Board, PipsList, [], false, Moves, 24)
    end.

find_normal_moves(_Board, _PipsList = [], _FailedPipsList = [], _PreBearOffMode, Moves, _Pos) ->
    Moves;
find_normal_moves(_Board, _PipsList, _FailedPipsList, _PreBearOffMode, Moves, _Pos = 0) ->
    Moves;
find_normal_moves(Board, _PipsList = [], FailedPipsList, PreBearOffMode, Moves, Pos) ->
    find_normal_moves(Board, FailedPipsList, [], PreBearOffMode, Moves, Pos - 1);
find_normal_moves(Board, [Pips | Rest] = PipsList, FailedPipsList, PreBearOffMode, Moves, Pos) ->
    case get_checkers(Pos, Board) of
        {?WHITE, _} ->
            BearOffMode = PreBearOffMode orelse bearoff_mode(Board), %% Optimization
            case check_move(Pos, Pips, Board, BearOffMode) of
                {Type, To} ->
                    NewBoard = apply_move(?WHITE, Pos, To, Type, Board),
                    find_normal_moves(NewBoard, Rest, FailedPipsList, BearOffMode, [{Pos, To} | Moves], Pos);
                error ->
                    find_normal_moves(Board, Rest, [Pips | FailedPipsList], BearOffMode, Moves, Pos)
            end;
        _ ->
            find_normal_moves(Board, PipsList, FailedPipsList, PreBearOffMode, Moves, Pos - 1)
    end.

bearoff_mode(Board) ->
    F = fun(Pos) -> not is_white(Pos, Board) end,
    lists:all(F, [?WHITE_BAR | lists:seq(7, 24)]).

is_white(Pos, Board) ->
    case get_checkers(Pos, Board) of
        {?WHITE, _} -> true;
        _ -> false
    end.

%% check_move(From, Pips, Board, BearOffMode) -> {move, To} | {hit, To} | error
check_move(From, Pips, Board, BearOffMode) ->
    To = new_pos(From, Pips),
    if To == ?WHITE_OUT andalso BearOffMode ->
           case no_white_checkers_behind(From, Board) of
               true -> {move, To};
               false -> error
           end;
       To == ?WHITE_OUT -> error;
       true ->
           case can_move_to(To, Board) of
               {yes, normal} -> {move, To};
               {yes, hit} -> {hit, To};
               no -> error
           end
    end.

no_white_checkers_behind(From, Board) ->
    if From == 6 -> true;
       true ->
           F = fun(Pos) -> not is_white(Pos, Board) end,
           lists:all(F, lists:seq(From + 1, 6))
    end.


can_move_to(Pos, Board) ->
    case get_checkers(Pos, Board) of
        empty -> {yes, normal};
        {?WHITE, _} -> {yes, normal};
        {?BLACK, 1} -> {yes, hit};
        {?BLACK, _} -> no
    end.

new_pos(Pos, Pips) ->
    case Pos of
        ?WHITE_BAR -> 25 - Pips;
        _ when is_integer(Pos) ->
            Diff = Pos - Pips,
            if Diff =< 0 -> ?WHITE_OUT;
               Diff > 0 -> Diff
            end
    end.

reverse_board(Board) ->
    [{reverse_pos(Pos), reverse_value(Value)} || {Pos, Value} <- Board].

reverse_moves(Moves) ->
    [{reverse_pos(From), reverse_pos(To)} || {From, To} <- Moves].

reverse_pos(Pos) ->
    case Pos of
        ?WHITE_OUT -> ?BLACK_OUT;
        ?BLACK_OUT -> ?WHITE_OUT;
        ?WHITE_BAR -> ?BLACK_BAR;
        ?BLACK_BAR -> ?WHITE_BAR;
        _ -> 25 - Pos
    end.

reverse_value(Value) ->
    case Value of
        empty -> empty;
        {Color, Num} -> {opponent_color(Color), Num}
    end.

