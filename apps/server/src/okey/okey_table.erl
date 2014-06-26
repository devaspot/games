%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description :
%%%
%%% Created : Oct 15, 2012
%%% -------------------------------------------------------------------
-module(okey_table).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("server/include/basic_types.hrl").
-include_lib("server/include/settings.hrl").
-include_lib("server/include/game_okey.hrl").
-include_lib("server/include/game_state.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("db/include/journal.hrl").
-include_lib("kvs/include/user.hrl").

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


-type tash() :: false_okey | {integer(), integer()}.

-record(desk_state,
        {
         state              :: state_take | state_discard | state_finished,
         hands              :: list({integer(), list(tash())}), %% {SeatNum, Tashes}
         discarded          :: list({integer(), list(tash())}), %% {SeatNum, Tashes}
         deck               :: list(tash()),
         cur_seat           :: integer(),
         gosterge           :: tash(),
         okey               :: tash(),
         has_gosterge       :: undefined | integer(), %% Seat num of a player who has gosterge
         have_8_tashes      :: list(integer()), %% Seats of players who show 8 tashes combination
         finish_reason      :: tashes_out | reveal | gosterge_finish, %% Defined only when state = state_finished
         finish_info        :: term()
        }).

-record(player, {?PLAYER}).

-define(STATE_WAITING_FOR_START, state_waiting_for_start).
-define(STATE_PLAYING, state_playing).
-define(STATE_REVEAL_CONFIRMATION, state_reveal_confirmation).
-define(STATE_FINISHED, state_finished).
-define(STATE_PAUSE, state_pause).
-define(STATE_SET_FINISHED, state_set_finished).

-define(HAND_SIZE, 14).
-define(SEATS_NUM, 4).
-define(RELAY, relay).
-define(DESK, okey_desk).
-define(GAME_STATS, journal).
-define(SCORING, okey_scoring).

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
    RevConfirmTimeout = proplists:get_value(reveal_confirmation_timeout, Params, get_timeout(challenge, Speed)), %% TODO Set this param explictly
    ReadyTimeout = proplists:get_value(ready_timeout, Params, get_timeout(ready, Speed)), %% TODO Set this param explictly
    RoundTimeout = proplists:get_value(round_timeout, Params),
    SetTimeout = proplists:get_value(set_timeout, Params),
    GameMode = proplists:get_value(game_type, Params),
    Rounds = proplists:get_value(rounds, Params),
    RevealConfirmation = proplists:get_value(reveal_confirmation, Params),
    NextSeriesConfirmation = proplists:get_value(next_series_confirmation, Params),
    PauseMode = proplists:get_value(pause_mode, Params),
    GostergeFinishAllowed = proplists:get_value(gosterge_finish_allowed, Params),
    SocialActionsEnabled = proplists:get_value(social_actions_enabled, Params),
    TTable = proplists:get_value(ttable, Params),
    Tour = proplists:get_value(tour, Params),
    Tours = proplists:get_value(tours, Params),
    %% Next two options will be passed on table respawn (after fail or service maintaince)
    ScoringState = proplists:get_value(scoring_state, Params, init_scoring(GameMode, PlayersInfo, Rounds)),
    CurRound = proplists:get_value(cur_round, Params, 0),

    Players = init_players(PlayersInfo),
    RelayParams = [{players, [{PlayerId, UserInfo#'PlayerInfo'.id} || {PlayerId, UserInfo, _, _} <- PlayersInfo]},
                   {observers_allowed, false},
                   {table, {?MODULE, self()}}],
    {ok, Relay} = ?RELAY:start(RelayParams),

    gas:info(?MODULE,"OKEY_NG_TABLE_TRN_DBG <~p,~p> Set timeout: ~p, round timeout: ~p.", [GameId, TableId, SetTimeout, RoundTimeout]),
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN_DBG <~p,~p> PlayersInfo: ~p.", [GameId, TableId, PlayersInfo]),
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Started.", [GameId, TableId]),
    parent_notify_table_created(Parent, TableId, Relay),
    {ok, ?STATE_WAITING_FOR_START, #okey_state{game_id = GameId,
                                          table_id = TableId,
                                          table_name = TableName,
                                          parent = Parent,
                                          relay = Relay,
                                          mult_factor = MultFactor,
                                          slang_flag = SlangFlag,
                                          observer_flag = ObserversFlag,
                                          tournament_type = TournamentType,
                                          tour = Tour,
                                          tours = Tours,
                                          speed = Speed,
                                          turn_timeout = TurnTimeout,
                                          reveal_confirmation_timeout = RevConfirmTimeout,
                                          ready_timeout = ReadyTimeout,
                                          round_timeout = RoundTimeout,
                                          set_timeout = SetTimeout,
                                          game_mode = GameMode,
                                          rounds = Rounds,
                                          reveal_confirmation = RevealConfirmation,
                                          next_series_confirmation = NextSeriesConfirmation,
                                          pause_mode = PauseMode,
                                          gosterge_finish_allowed = GostergeFinishAllowed,
                                          social_actions_enabled = SocialActionsEnabled,
                                          players = Players,
                                          start_seat = crypto:rand_uniform(1, ?SEATS_NUM + 1),
                                          cur_round = CurRound,
                                          scoring_state = ScoringState,
                                          tournament_table = TTable
                                         }}.

handle_event({parent_message, Message}, StateName,
             #okey_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received message from the parent: ~p.",
          [GameId, TableId, Message]),
    handle_parent_message(Message, StateName, StateData);

handle_event({relay_message, Message}, StateName,
             #okey_state{game_id = GameId, table_id = TableId} =  StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received message from the relay: ~p.",
          [GameId, TableId, Message]),
    handle_relay_message(Message, StateName, StateData);

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

handle_sync_event({player_action, PlayerId, Action}, From, StateName,
                  #okey_state{players = Players} = StateData) ->
    case get_player(PlayerId, Players) of
        {ok, Player} ->
            handle_player_action(Player, Action, From, StateName, StateData);
        error ->
            {reply, {error, you_are_not_a_player}, StateName, StateData}
    end;

handle_sync_event(_Event, _From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

handle_info({timeout, Magic}, ?STATE_PLAYING,
            #okey_state{timeout_magic = Magic, game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Move timeout. Do an automatic move(s).", [GameId, TableId]),
    do_timeout_moves(StateData);

handle_info({timeout, Magic}, State,
            #okey_state{timeout_magic = Magic, game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Move timeout. Do an automatic move(s). UNKNOWN STATE ~p", [GameId, TableId,State]),
    do_timeout_moves(StateData);

handle_info({round_timeout, Round}, ?STATE_PLAYING,
            #okey_state{cur_round = Round, desk_state = DeskState, game_id = GameId,
                   table_id = TableId, timeout_timer = TRef} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Time to finish round ~p because the round timeout.", [GameId, TableId, Round]),
    if TRef =/= undefined -> erlang:cancel_timer(TRef);
       true -> do_nothing
    end,
    finalize_round(StateData#okey_state{desk_state = DeskState#desk_state{finish_reason = timeout}});

handle_info(set_timeout, StateName,
            #okey_state{cur_round = Round, desk_state = DeskState, game_id = GameId,
                   table_id = TableId, timeout_timer = TRef} = StateData) when
  StateName =/= ?STATE_SET_FINISHED ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Time to finish round ~p and the set because the set timeout.", [GameId, TableId, Round]),
    if TRef =/= undefined -> erlang:cancel_timer(TRef);
       true -> do_nothing
    end,
    finalize_round(StateData#okey_state{desk_state = DeskState#desk_state{finish_reason = set_timeout}});

handle_info({timeout, Magic}, ?STATE_REVEAL_CONFIRMATION,
            #okey_state{timeout_magic = Magic, wait_list = WL, game_id = GameId, table_id = TableId,
                   reveal_confirmation_list = CList} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Time to check reveal confirmation responses.", [GameId, TableId]),
    NewCList = lists:foldl(fun(SeatNum, Acc) -> [{SeatNum, false} | Acc] end, CList, WL),
    finalize_round(StateData#okey_state{reveal_confirmation_list = NewCList});

handle_info(Info, StateName, #okey_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Unexpected message(info) received at state <~p>: ~p.",
          [GameId, TableId, StateName, Info]),
    {next_state, StateName, StateData}.

terminate(Reason, StateName, #okey_state{game_id = GameId, table_id = TableId, relay = Relay}) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Shutting down at state: <~p>. Reason: ~p",
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
                      #okey_state{table_id = TableId, players = Players,
                             parent = Parent, relay = Relay} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    NewPlayers = reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, _Connected = false, Players),
    relay_register_player(Relay, UserId, PlayerId),
    %% TODO: Send notificitations to gamesessions (we have no such notification)
    parent_confirm_registration(Parent, TableId, RequestId),
    {next_state, StateName, StateData#okey_state{players = NewPlayers}};

handle_parent_message({replace_player, RequestId, UserInfo, PlayerId, SeatNum}, StateName,
                      #okey_state{table_id = TableId, players = Players,
                             parent = Parent, relay = Relay} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = IsBot} = UserInfo,
    #player{id = OldPlayerId} = get_player_by_seat_num(SeatNum, Players),
    NewPlayers = del_player(OldPlayerId, Players),
    NewPlayers2 = reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, _Connected = false, NewPlayers),
    relay_kick_player(Relay, OldPlayerId),
    relay_register_player(Relay, UserId, PlayerId),
    ReplaceMsg = create_player_left(SeatNum, UserInfo, Players),
    relay_publish_ge(Relay, ReplaceMsg, StateData),
    parent_confirm_replacement(Parent, TableId, RequestId),
    {next_state, StateName, StateData#okey_state{players = NewPlayers2}};

handle_parent_message(start_round, StateName,
                      #okey_state{game_mode = GameMode, cur_round = CurRound,
                             gosterge_finish_allowed = GostergeFinishAllowed,
                             start_seat = LastStartSeat, players = Players,
                             relay = Relay, turn_timeout = TurnTimeout,
                             round_timeout = RoundTimeout, set_timeout = SetTimeout,
                             set_timer = SetTRef, scoring_state = ScoringState} = StateData)
  when StateName == ?STATE_WAITING_FOR_START;
       StateName == ?STATE_FINISHED ->
    NewCurRound = CurRound + 1,
    StartSeat = next_seat_num(LastStartSeat),
    Deck = deck:shuffle(deck:init_deck(okey)),
    {Gosterge, Deck1} = choose_gosterge(Deck),
    F = fun(SeatNum, AccDeck) ->
                Num = if SeatNum==StartSeat -> ?HAND_SIZE + 1; true -> ?HAND_SIZE end,
                lists:split(Num, AccDeck)
        end,
    {Hands, TablePile} = lists:mapfoldl(F, deck:to_list(Deck1), lists:seq(1, ?SEATS_NUM)),
    GostFinishList = if GameMode == countdown andalso GostergeFinishAllowed andalso NewCurRound > 1 ->
                            {_,_,_,Scores} = ?SCORING:last_round_result(ScoringState),
                            [SeatNum || {SeatNum, 1} <- Scores];
                        true -> []
                     end,
    Have8TashesEnabled = GameMode == evenodd orelse GameMode == color,
    Params = [{hands, Hands},
              {deck, TablePile},
              {gosterge, Gosterge},
              {cur_player, StartSeat},
              {gosterge_finish_list, GostFinishList},
              {have_8_tashes_enabled, Have8TashesEnabled}],
    {ok, Desk} = ?DESK:start(Params),
    DeskState = init_desk_state(Desk),
    %% Init timers
    {Magic, TRef} = start_timer(TurnTimeout),
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
    NewStateData = StateData#okey_state{cur_round = NewCurRound,
                                   start_seat = StartSeat,
                                   desk_rule_pid = Desk,
                                   desk_state = DeskState,
                                   timeout_timer = TRef,
                                   timeout_magic = Magic,
                                   round_timer = RoundTRef,
                                   set_timer = NewSetTRef},

    [begin
         GameInfoMsg = create_okey_game_info(NewStateData),
         send_to_client_ge(Relay, PlayerId, GameInfoMsg, NewStateData),
         GameStartedMsg = create_okey_game_started(SeatNum, DeskState, NewCurRound, NewStateData),
         send_to_client_ge(Relay, PlayerId, GameStartedMsg, NewStateData)
     end || #player{id = PlayerId, seat_num = SeatNum} <- find_connected_players(Players)],
    CurSeatNum = DeskState#desk_state.cur_seat,
    relay_publish_ge(Relay, create_okey_next_turn(CurSeatNum, Players, false), NewStateData),
    {next_state, ?STATE_PLAYING, NewStateData};

handle_parent_message(show_round_result, StateName,
                      #okey_state{relay = Relay, scoring_state = ScoringState,
                             game_id = GameId, table_id = TableId} = StateData) ->
    {FinishInfo, RoundScore, AchsPoints, TotalScore} = ?SCORING:last_round_result(ScoringState),
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> RoundScore: ~p Total score: ~p.", [GameId, TableId, RoundScore, TotalScore]),
    {Reason,Revealer,RevealerWon,WrongRejects} = case FinishInfo of
        {win_reveal, R, Wrong, _Color, _Okey, _Pairs} -> {win_reveal,R,true,Wrong};
        {fail_reveal, R} -> {fail_reveal,R,false,[]};
%        {fail_reveal, R} -> {tashes_out,none,false,[]}; % check tashes out event
        tashes_out -> {tashes_out,none,false,[]};
        timeout -> {timeout,none,false,[]};
        set_timeout -> {timeout,none,false,[]};
        {gosterge_finish, Winner} -> {gosterge_finish,Winner,true,[]} end,
    Msg = round_results(Reason,Revealer,RevealerWon,WrongRejects,RoundScore,TotalScore,AchsPoints,StateData),
    relay_publish_ge(Relay, Msg, StateData),
    {next_state, StateName, StateData#okey_state{}};

%% Results = [{PlayerId, Position, Score, Status}] Status = winner | loser | eliminated | none
handle_parent_message({show_series_result, Results}, StateName,
                      #okey_state{relay = Relay, players = Players,
                             next_series_confirmation = Confirm} = StateData) ->
    Msg = create_okey_series_ended(Results, Players, Confirm, StateData),
    relay_publish_ge(Relay, Msg, StateData),
    {next_state, StateName, StateData#okey_state{}};

%% Results = [{UserId, Position, Score, Status}] Status = active | eliminated
handle_parent_message({tour_result, TourNum, Results}, StateName,
                      #okey_state{relay = Relay, tournament_table = TTable} = StateData) ->
    NewTTable = [{TourNum, Results} | TTable],
    Msg = create_okey_tour_result(TourNum, Results),
    relay_publish_ge(Relay, Msg, StateData),
    {next_state, StateName, StateData#okey_state{tournament_table = NewTTable}};

handle_parent_message({playing_tables_num, Num}, StateName,
                      #okey_state{relay = Relay} = StateData) ->
%%XXX    Msg = create_okey_playing_tables(Num),
%%    relay_publish_ge(Relay, Msg),
    {next_state, StateName, StateData};

handle_parent_message(rejoin_players, StateName,
                      #okey_state{game_id = GameId, relay = Relay,
                             players = Players} = StateData) ->
    [relay_unregister_player(Relay, P#player.id, {rejoin, GameId}) || P <- players_to_list(Players)],
    {next_state, StateName, StateData#okey_state{players = players_init()}};


handle_parent_message(disconnect_players, StateName,
                      #okey_state{relay = Relay, players = Players} = StateData) ->
    [relay_unregister_player(Relay, P#player.id, game_over) || P <- players_to_list(Players)],
    {next_state, StateName, StateData#okey_state{players = players_init()}};


handle_parent_message(stop, _StateName, StateData) ->
    {stop, normal, StateData};

handle_parent_message(Message, StateName,
                      #okey_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:error(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Unexpected parent message received in state <~p>: ~p. State: ~p. Stopping.",
           [GameId, TableId, StateName, Message, StateName]),
    {stop, unexpected_parent_message, StateData}.


%%===================================================================

%% handle_relay_message(Msg, StateName, StateData)

handle_relay_message({player_connected, PlayerId} = Msg, StateName,
                     #okey_state{parent = Parent, game_id = GameId,
                            table_id = TableId, players = Players} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received nofitication from the relay: ~p", [GameId, TableId, Msg]),
    case get_player(PlayerId, Players) of
        {ok, Player} ->
            NewPlayers = store_player_rec(Player#player{connected = true}, Players),
            parent_send_player_connected(Parent, TableId, PlayerId),
            {next_state, StateName, StateData#okey_state{players = NewPlayers}};
        error ->
            {next_state, StateName, StateData}
    end;


handle_relay_message({player_disconnected, PlayerId}, StateName,
                     #okey_state{parent = Parent, table_id = TableId, players = Players} = StateData) ->
    case get_player(PlayerId, Players) of
        {ok, Player} ->
            NewPlayers = store_player_rec(Player#player{connected = false}, Players),
            parent_send_player_disconnected(Parent, TableId, PlayerId),
            {next_state, StateName, StateData#okey_state{players = NewPlayers}};
        error ->
            {next_state, StateName, StateData}
    end;

handle_relay_message({subscriber_added, PlayerId, SubscrId} = Msg, StateName,
                     #okey_state{relay = Relay, game_id = GameId,
                            table_id = TableId, tournament_table = TTable,
                            players = Players} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received nofitication from the relay: ~p", [GameId, TableId, Msg]),
    PlayerIdIsValid = case PlayerId of
                          observer -> true;
                          administrator -> true;
                          _ ->
                              case get_player(PlayerId, Players) of
                                  {ok, _} -> true;
                                  error -> false
                              end
                      end,
    if PlayerIdIsValid ->

        GI = create_okey_game_info(StateData),
        send_to_subscriber_ge(Relay, SubscrId, GI, StateData),

           PlState = create_okey_game_player_state(PlayerId, StateName, StateData),
           send_to_subscriber_ge(Relay, SubscrId, PlState, StateData),
           relay_allow_broadcast_for_player(Relay, PlayerId),
           if TTable =/= undefined ->
                  [send_to_subscriber_ge(Relay, SubscrId, create_okey_tour_result(TurnNum, Results), StateData)
                     || {TurnNum, Results} <- lists:sort(TTable)];
              true -> do_nothing
           end;
       true -> do_nothing
    end,
    {next_state, StateName, StateData};

handle_relay_message(Message, StateName, #okey_state{game_id = GameId, table_id = TableId} = StateData) ->
    gas:error(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Unknown relay message received in state <~p>: ~p. State: ~p. Stopping.",
           [GameId, TableId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================

%% handle_player_action(Player, Msg, StateName, StateData)

handle_player_action(#player{id = PlayerId, seat_num = SeatNum, user_id = UserId},
                     {submit, #game_action{action = Action, args = Args} = GA}, From,
                     StateName,
                     #okey_state{game_id = GameId, table_id = TableId} = StateData) ->
    {Keys,Values} = lists:unzip(Args),
    ExtAction = list_to_tuple([Action|Values]),
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Player <~p> (~p)~n submit the game action: ~p.",
        [GameId, TableId, PlayerId, UserId, ExtAction]),
    do_action(SeatNum, ExtAction, From, StateName, StateData);

handle_player_action(#player{id = PlayerId, user_id = UserId},
                     {signal, {pause_game, _}=Signal}, _From,
                     StateName,
                     #okey_state{table_id = TableId, game_id = GameId, timeout_timer = TRef,
                            pause_mode = PauseMode, relay = Relay} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received signal from player <~p> : ~p. PauseMode: ~p",
          [GameId, TableId, PlayerId, Signal, PauseMode]),
    case PauseMode of
        disabled ->
            {reply, {error, pause_disabled}, StateName, StateData};
        normal ->
            if StateName == ?STATE_PLAYING;
               StateName == ?STATE_REVEAL_CONFIRMATION ->
                   Timeout = case erlang:cancel_timer(TRef) of
                                 false -> 0;
                                 T -> T
                             end,
                   relay_publish(Relay, create_game_paused_pause(UserId, GameId)),
                   {reply, 0, ?STATE_PAUSE, StateData#okey_state{paused_statename = StateName,
                                                            paused_timeout_value = Timeout,
                                                            timeout_magic = undefined}};
               true ->
                   {reply, {error, pause_not_possible}, StateName, StateData}
            end
    end;


handle_player_action(#player{id = PlayerId, user_id = UserId},
                     {signal, {resume_game, _}=Signal}, _From,
                     StateName,
                     #okey_state{table_id = TableId, game_id = GameId, pause_mode = PauseMode,
                            relay = Relay, paused_statename = ResumedStateName,
                            paused_timeout_value = Timeout} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received signal from player <~p> : ~p. PauseMode: ~p",
          [GameId, TableId, PlayerId, Signal, PauseMode]),
    case PauseMode of
        disabled ->
            {reply, {error, pause_disabled}, StateName, StateData};
        normal ->
            if StateName == ?STATE_PAUSE ->
                   relay_publish(Relay, create_game_paused_resume(UserId, GameId)),
                   {Magic, TRef} = start_timer(Timeout),
                   {reply, 0, ResumedStateName, StateData#okey_state{timeout_timer = TRef,
                                                                timeout_magic = Magic}};
               true ->
                   {reply, {error, game_is_not_paused}, StateName, StateData}
            end
    end;


handle_player_action(#player{id = PlayerId},
                     {signal, Signal}, _From, StateName,
                     #okey_state{table_id = TableId, game_id = GameId} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Received signal from player <~p> : ~p. Ignoring.",
          [GameId, TableId, PlayerId, Signal]),
    {reply, ok, StateName, StateData};


handle_player_action(_Player, _Message, _From, StateName, StateData) ->
    {next_state, StateName, StateData}.



%%===================================================================

do_action(SeatNum, #okey_has_gosterge{}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, i_have_gosterge, From, StateName, StateData);

do_action(_SeatNum, #okey_has_gosterge{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_i_have_8_tashes{}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, i_have_8_tashes, From, StateName, StateData);

do_action(_SeatNum, #okey_i_have_8_tashes{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_i_saw_okey{}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, see_okey, From, StateName, StateData);

do_action(_SeatNum, #okey_i_saw_okey{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_take{pile = 0}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, take_from_table, From, StateName, StateData);

do_action(_SeatNum, #okey_take{pile = 0}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_take{pile = 1}, From, ?STATE_PLAYING = StateName, StateData) ->
    do_game_action(SeatNum, take_from_discarded, From, StateName, StateData);

do_action(_SeatNum, #okey_take{pile = 1}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_discard{tile = ExtTash}, From, ?STATE_PLAYING = StateName, StateData) ->
    Tash = ext_to_tash(ExtTash),
    do_game_action(SeatNum, {discard, Tash}, From, StateName, StateData);

do_action(_SeatNum, #okey_discard{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_reveal{discarded = ExtDiscarded, hand = ExtHand}, From,
          ?STATE_PLAYING = StateName, StateData) ->
    Discarded = ext_to_tash(ExtDiscarded),
    Hand = [[if ExtTash == null -> null;
                true -> ext_to_tash(ExtTash)
             end || ExtTash <- Row] || Row <- ExtHand],

    DeskState = StateData#okey_state.desk_state,
    Gosterme = DeskState#desk_state.gosterge,

%    CheckHand = [[{3,12},{2,12},{1,12},{2,9},{2,7},{2,8},{2,6},{2,4},
%                    {4,8},{4,7},{4,9},{3,12},{3,13},{3,1}],[]],
%    {Revealed,_,_,NormalizedHand} = ?SCORING:check_reveal(CheckHand,{2,3}),

    {Revealed,_,_,Normalized} = ?SCORING:check_reveal(Hand,Gosterme),

    case Revealed of
        true -> 
            Denormalized = case Normalized of
                [] -> Hand;
                _ -> okey_scoring:denormalize_reveal(Normalized,Gosterme) end,
            wf:info(?MODULE,"Denormalized Reveal ~p",[Denormalized]),
            do_game_action(SeatNum, {reveal, Discarded, Denormalized}, From, StateName, StateData);
        false -> do_game_action(SeatNum, wrong_reveal, From, StateName, StateData)
    end;

do_action(_SeatNum, #okey_reveal{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(SeatNum, #okey_challenge{challenge = Challenge}, From,
          ?STATE_REVEAL_CONFIRMATION = StateName, #okey_state{reveal_confirmation_list = CList,
                                                         wait_list = WL,
                                                         timeout_timer = TRef} = StateData) ->
    case lists:member(SeatNum, WL) of
        true ->
            Confirmed = not Challenge,
            NewCList = [{SeatNum, Confirmed} | CList],
            NewWL = lists:delete(SeatNum, WL),
            if NewWL == [] ->
                   gen_fsm:reply(From, ok),
                   erlang:cancel_timer(TRef),
                   finalize_round(StateData#okey_state{timeout_timer = undefined,
                                                  reveal_confirmation_list = NewCList});
               true ->
                   {reply, ok, StateName,
                    StateData#okey_state{reveal_confirmation_list = NewCList,
                                    wait_list = NewWL}}
            end;
        false ->
            {reply, {error, not_your_turn}, StateName, StateData}
    end;

do_action(_SeatNum, #okey_challenge{}, _From, StateName, StateData) ->
    {reply, {error, message_not_valid_for_a_current_state}, StateName, StateData};


do_action(_SeatNum, #okey_ready{}, _From, StateName, StateData) ->
    {reply, ok, StateName, StateData};


do_action(_SeatNum, _UnsupportedAction, _From, StateName, StateData) ->
    {reply, {error, unsupported}, StateName, StateData}.


%%===================================================================
do_timeout_moves(#okey_state{desk_rule_pid = Desk, desk_state = DeskState} = StateData) ->
    #desk_state{cur_seat = CurSeatNum,
                hands = Hands,
                state = DeskStateName} = DeskState,
    case DeskStateName of
        state_take ->
            {ok,Events1} = desk_player_action(Desk, CurSeatNum, take_from_table),
            [Tash] = [Tash || {taked_from_table, S, Tash} <- Events1, S==CurSeatNum],
            {ok, Events2} = desk_player_action(Desk, CurSeatNum, {discard, Tash}),
            Events2_1 = [case E of
                             {tash_discarded, SeatNum, Tash} ->
                                 {tash_discarded_timeout, SeatNum, Tash};
                             _ -> E
                         end || E <- Events2],
            Events = Events1 ++ [{auto_take_discard, CurSeatNum, Tash}] ++ Events2_1,
            process_game_events(Events, StateData);
        state_discard ->
            {_, [Tash | _]} = lists:keyfind(CurSeatNum, 1, Hands),
            {ok,Events1} = desk_player_action(Desk, CurSeatNum, {discard, Tash}),
            Events1_1 = [case E of
                             {tash_discarded, SeatNum, Tash} ->
                                 {tash_discarded_timeout, SeatNum, Tash};
                             _ -> E
                         end || E <- Events1],
            Events = [{auto_discard, CurSeatNum, Tash} | Events1_1],
            process_game_events(Events, StateData)
    end.

%%===================================================================

do_game_action(SeatNum, GameAction, From, StateName,
               #okey_state{
              desk_state = #desk_state{okey=Okey},
              desk_rule_pid = Desk} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN do_game_action SeatNum: ~p  GameAction: ~p", [SeatNum, GameAction]),
    case desk_player_action(Desk, SeatNum, GameAction) of
        {ok, Events} ->
            Response = case GameAction of
                           i_have_gosterge ->
                               true;
                           i_have_8_tashes ->
                               true;
                           take_from_table ->
                               [Tash] = [Tash || {taked_from_table, S, Tash} <- Events, S==SeatNum],
                               tash_to_ext(Tash,Okey);
                           take_from_discarded ->
                               [Tash] = [Tash || {taked_from_discarded, S, Tash} <- Events, S==SeatNum],
                               tash_to_ext(Tash,Okey);
                           _ -> ok
                       end,
            gen_fsm:reply(From, Response),
            process_game_events(Events, StateData);
        {error, Reason} ->
            wf:info(?MODULE,"do game action error: ~p",[{Reason,GameAction}]),
            ExtError = desk_error_to_ext(Reason),
            {reply, ExtError, StateName, StateData}
    end.


process_game_events(Events, #okey_state{desk_state = DeskState, players = Players,
                                   relay = Relay, timeout_timer = OldTRef,
                                   round_timeout = RoundTimeout, round_timer = RoundTRef,
                                   turn_timeout = TurnTimeout} = StateData) ->
    NewDeskState = handle_desk_events(Events, DeskState, Players, Relay, StateData), %% Track the desk and send game events to clients
    #desk_state{state = DeskStateName} = NewDeskState,
    case DeskStateName of
        state_finished ->
            if is_integer(RoundTimeout) -> erlang:cancel_timer(RoundTRef); true -> do_nothing end,
            erlang:cancel_timer(OldTRef),
            on_game_finish(StateData#okey_state{desk_state = NewDeskState});
        state_take ->
            case [E || {next_player, _, _} = E <- Events] of %% Find a next player event
                [] ->
                    {next_state, ?STATE_PLAYING, StateData#okey_state{desk_state = NewDeskState}};
                [_|_] ->
                    erlang:cancel_timer(OldTRef),
                    {Magic, TRef} = start_timer(TurnTimeout),
                    {next_state, ?STATE_PLAYING, StateData#okey_state{desk_state = NewDeskState,
                                                                 timeout_timer = TRef,
                                                                 timeout_magic = Magic}}
            end;
        state_discard ->
            {next_state, ?STATE_PLAYING, StateData#okey_state{desk_state = NewDeskState}}
    end.


on_game_finish(#okey_state{desk_state = DeskState,
                      reveal_confirmation = RevealConfirmation,
                      reveal_confirmation_timeout = Timeout} = StateData) ->
    #desk_state{finish_reason = FinishReason,
                finish_info = FinishInfo,
                hands = Hands} = DeskState,
    if FinishReason == reveal andalso RevealConfirmation ->
           {Revealer, _Tashes, _Discarded} = FinishInfo,
           WL = [SeatNum || {SeatNum, _} <- Hands, SeatNum =/= Revealer],
           {Magic, TRef} = start_timer(Timeout),
           {next_state, ?STATE_REVEAL_CONFIRMATION,
            StateData#okey_state{reveal_confirmation_list = [],
                            wait_list = WL,
                            timeout_timer = TRef,
                            timeout_magic = Magic}};
       true ->
            finalize_round(StateData)
    end.

%%===================================================================

finalize_round(#okey_state{desk_state = #desk_state{finish_reason = FinishReason,
                                               finish_info = FinishInfo,
                                               hands = Hands,
                                               gosterge = Gosterge,
                                               has_gosterge = WhoHasGosterge,
                                               have_8_tashes = Have8Tashes},
                      scoring_state = ScoringState,
                      reveal_confirmation = RevealConfirmation,
                      reveal_confirmation_list = CList,
                      parent = Parent, players = Players,
                      game_id = GameId, table_id = TableId} = StateData) ->
    gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Finalizing the round. Finish reason: ~p. Finish info: ~p.",
          [GameId, TableId, FinishReason, FinishInfo]),
    FR = case FinishReason of
             tashes_out -> tashes_out;
             timeout -> timeout;
             set_timeout -> set_timeout;
             reveal ->
                 {Revealer, Tashes, Discarded} = FinishInfo,
                 ConfirmationList = if RevealConfirmation -> CList; true -> [] end,
                 CListUId = [{SeatNum, get_user_id_by_seat_num(SeatNum, Players), Response}
                             || {SeatNum, Response} <- ConfirmationList],
                 gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Confirmation list: ~p.", [GameId, TableId, CListUId]),
                 {reveal, Revealer, Tashes, Discarded, ConfirmationList};
             gosterge_finish ->
                 Winner = FinishInfo,
                 {gosterge_finish, Winner}
         end,
    {NewScoringState, GameOver} = ?SCORING:round_finished(ScoringState, FR, Hands, Gosterge,
                                                          WhoHasGosterge, Have8Tashes),
    {_, RoundScore, _, TotalScore} = ?SCORING:last_round_result(NewScoringState),
    RoundScorePl = [{get_player_id_by_seat_num(SeatNum, Players), Points} || {SeatNum, Points} <- RoundScore],
    TotalScorePl = [{get_player_id_by_seat_num(SeatNum, Players), Points} || {SeatNum, Points} <- TotalScore],
    if GameOver ->
           gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Set is over.", [GameId, TableId]),
           parent_send_game_res(Parent, TableId, NewScoringState, RoundScorePl, TotalScorePl),
           {next_state, ?STATE_SET_FINISHED, StateData#okey_state{scoring_state = NewScoringState}};
       true ->
           gas:info(?MODULE,"OKEY_NG_TABLE_TRN <~p,~p> Round is over.", [GameId, TableId]),
           parent_send_round_res(Parent, TableId, NewScoringState, RoundScorePl, TotalScorePl),
           {next_state, ?STATE_FINISHED, StateData#okey_state{scoring_state = NewScoringState}}
    end.



%% handle_desk_events(Events, DeskState, Players) -> NextStateData
%% Tracks the desk state and sends events to clients
handle_desk_events([], DeskState, _Players, _Relay, _StateData) ->
    DeskState;

handle_desk_events([Event | Events], DeskState, Players, Relay, #okey_state{} = StateData) ->
    #desk_state{cur_seat = CurSeatNum,
                hands = Hands,
                discarded = Discarded,
                okey = Okey,
                deck = Deck,
                have_8_tashes = Have8Tashes} = DeskState,
    NewDeskState =
        case Event of
            {has_gosterge, SeatNum} ->
                Msg = create_okey_player_has_gosterge(SeatNum, Players),
                relay_publish_ge(Relay, Msg, StateData),
                DeskState#desk_state{has_gosterge = SeatNum};
            {has_8_tashes, SeatNum, Value} ->
                Msg = create_okey_player_has_8_tashes(SeatNum, Value, Players),
                relay_publish_ge(Relay, Msg, StateData),
                DeskState#desk_state{have_8_tashes = [SeatNum | Have8Tashes]};
            {saw_okey, SeatNum} ->
                Msg = create_okey_disable_okey(SeatNum, CurSeatNum, Players),
                relay_publish_ge(Relay, Msg, StateData),
                DeskState;
            {taked_from_discarded, SeatNum, Tash} ->
                PrevSeatNum = prev_seat_num(SeatNum),
                {_, [Tash | NewPile]} = lists:keyfind(PrevSeatNum, 1, Discarded),
                Msg = create_okey_tile_taken_discarded(SeatNum, Tash, length(NewPile), Players, Okey),
                relay_publish_ge(Relay, Msg, StateData),
                NewDiskarded = lists:keyreplace(PrevSeatNum, 1, Discarded, {PrevSeatNum, NewPile}),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, [Tash | Hand]}),
                DeskState#desk_state{hands = NewHands, discarded = NewDiskarded, state = state_discard};
            {taked_from_table, SeatNum, Tash} ->
                [Tash | NewDeck] = Deck,
                [ send_to_client_ge(Relay, Id,
                    create_okey_tile_taken_table(CSN, CurSeatNum, Tash, length(NewDeck), Players, Okey), StateData)
                || #player{id = Id,seat_num = CSN} <- find_connected_players(Players) ],
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, [Tash | Hand]}),
                DeskState#desk_state{hands = NewHands, deck = NewDeck, state = state_discard};
            {tash_discarded, SeatNum, Tash} ->
                Msg = create_okey_tile_discarded(SeatNum, Tash, false, Players, Okey),
                relay_publish_ge(Relay, Msg, StateData),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, lists:delete(Tash, Hand)}),
                {_, Pile} = lists:keyfind(SeatNum, 1, Discarded),
                NewDiscarded = lists:keyreplace(SeatNum, 1, Discarded, {SeatNum, [Tash | Pile]}),
                DeskState#desk_state{hands = NewHands, discarded = NewDiscarded, state = state_take};
            {tash_discarded_timeout, SeatNum, Tash} -> %% Injected event
                Msg = create_okey_tile_discarded(SeatNum, Tash, true, Players, Okey),
                relay_publish_ge(Relay, Msg, StateData),
                {_, Hand} = lists:keyfind(SeatNum, 1, Hands),
                NewHands = lists:keyreplace(SeatNum, 1, Hands, {SeatNum, lists:delete(Tash, Hand)}),
                {_, Pile} = lists:keyfind(SeatNum, 1, Discarded),
                NewDiscarded = lists:keyreplace(SeatNum, 1, Discarded, {SeatNum, [Tash | Pile]}),
                DeskState#desk_state{hands = NewHands, discarded = NewDiscarded, state = state_take};
            {auto_take_discard, SeatNum, Tash} ->    %% Injected event
                #player{id = PlayerId, user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_okey_turn_timeout(UserId, Tash, Tash, Okey),
                send_to_client_ge(Relay, PlayerId, Msg, StateData),
                DeskState;
            {auto_discard, SeatNum, Tash} ->         %% Injected event
                #player{id = PlayerId, user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_okey_turn_timeout(UserId, null, Tash, Okey),
                send_to_client_ge(Relay, PlayerId, Msg, StateData),
                DeskState;
            {next_player, SeatNum, EnableOkey} ->
                #player{id = PlayerId, user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_okey_next_turn(SeatNum, Players, EnableOkey),
                relay_publish_ge(Relay, Msg, StateData),
                DeskState#desk_state{cur_seat = SeatNum, state = state_take};
            no_winner_finish ->
                DeskState#desk_state{state = state_finished,
                                     finish_reason = tashes_out};
            {wrong_reveal,SeatNum} ->
                #player{id = PlayerId, user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_deny_wrong_reveal(),
                send_to_client_ge(Relay, PlayerId, Msg, StateData),
                DeskState;
            {reveal, SeatNum, RevealedTashes, DiscardedTash} ->
                #player{id = PlayerId, user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
                Msg = create_okey_revealed(SeatNum, DiscardedTash, RevealedTashes, Players, Okey),
                relay_publish_ge(Relay, Msg, StateData),
                DeskState#desk_state{state = state_finished,
                                     finish_reason = reveal,
                                     finish_info = {SeatNum, RevealedTashes, DiscardedTash}};
            {gosterge_finish, SeatNum} ->
                DeskState#desk_state{state = state_finished,
                                     finish_reason = gosterge_finish,
                                     finish_info = SeatNum}
        end,
    handle_desk_events(Events, NewDeskState, Players, Relay, StateData).

create_deny_wrong_reveal() -> #wrong_reveal{}.

%%===================================================================
init_scoring(GameType, PlayersInfo, Rounds) ->
    SeatsInfo = [{SeatNum, Points} || {_PlayerId, _UserInfo, SeatNum, Points} <- PlayersInfo],
    ?SCORING:init(GameType, SeatsInfo, Rounds).


%% start_timer(Timeout) -> {Magic, TRef}
start_timer(Timeout) ->
    Magic = make_ref(),
    TRef = erlang:send_after(Timeout, self(), {timeout, Magic}),
    {Magic, TRef}.

%% players_init() -> players()
players_init() ->
    midict:new().

%% reg_player(PlayerId, SeatNum, UserId, IsBot, Players) -> NewPlayers
reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, Connected, Players) ->
    store_player_rec(#player{id =PlayerId, seat_num = SeatNum, user_id = UserId,
                             is_bot = IsBot, info = UserInfo, connected = Connected}, Players).

%% reg_player(#player{}, Players) -> NewPlayers
store_player_rec(#player{id =Id, seat_num = SeatNum, user_id = UserId,
                         is_bot = IsBot, connected = Connected} = Player, Players) ->
    Indices = [{seat_num, SeatNum}, {user_id, UserId}, {is_bot, IsBot}, {connected, Connected}],
    midict:store(Id, Player, Indices, Players).

%% get_player_id_by_seat_num(SeatNum, Players) -> PlayerId
get_player_id_by_seat_num(SeatNum, Players) ->
    [#player{id = PlayerId}] = midict:geti(SeatNum, seat_num, Players),
    PlayerId.

%% get_user_id_by_seat_num(SeatNum, Players) -> PlayerId
get_user_id_by_seat_num(SeatNum, Players) ->
    [#player{user_id = UserId}] = midict:geti(SeatNum, seat_num, Players),
    UserId.

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

%% find_connected_players(Players) -> [Player]
find_connected_players(Players) ->
    midict:geti(true, connected, Players).

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
    NewPlayers = reg_player(PlayerId, SeatNum, UserId, IsBot, UserInfo, _Connected = false, Players),
    init_players(PlayersInfo, NewPlayers).

%%=================================================================

handle_log(User,#game_event{}=Event,
    #okey_state{game_id=GameId,tournament_type=GameKind,game_mode=GameMode,speed=Speed,rounds=Rounds}=State) ->
    ProtocolEvent = #protocol_event{feed_id=User,module=GameKind,speed=Speed,rounds=Rounds,user=User,
        type=GameMode,id=?GAME_STATS:timestamp(),event=Event#game_event.event,game_event=Event},
    ?GAME_STATS:update_stats(User,ProtocolEvent,#protocol_event.event,State).

send_to_subscriber_ge(Relay, SubscrId, Msg, #okey_state{players=Players,game_id = GameId} = State) ->
    [Name|List] = tuple_to_list(Msg),
    Event = #game_event{game = GameId, event = Name, args = lists:zip(known_records:fields(Name),List) },
%    gas:info(?MODULE,"SUBSCRIBER ~p",[SubscrId]),
    ?RELAY:table_message(Relay, {to_subscriber, SubscrId, Event}).

send_to_client_ge(Relay, PlayerId, Msg, #okey_state{players=Players,game_id = GameId} = State) ->
    [Name|List] = tuple_to_list(Msg),
    Event = #game_event{game = GameId, event = Name, args = lists:zip(known_records:fields(Name),List) },
%    gas:info(?MODULE,"SEND CLIENT ~p",[Event]),
    ?GAME_STATS:protocol_event(table,Event,State),
    case get_player(PlayerId, Players) of
        {ok, #player{user_id=User,is_bot=false}} -> handle_log(User,Event,State);
        _ -> skip end,
    ?RELAY:table_message(Relay, {to_client, PlayerId, Event}).

relay_publish_ge(Relay, Msg, #okey_state{players=Players,game_id = GameId} = State) ->
    [Name|List] = tuple_to_list(Msg),
    Event = #game_event{game = GameId, event = Name, args = lists:zip(known_records:fields(Name),List) },
%    gas:info(?MODULE,"RELAYX PUBLISH ~p",[Event]),
    ?GAME_STATS:protocol_event(table,Event,State),
    [ handle_log(UserId,Event,State) 
    || {_,#player{id=Id,user_id=UserId,is_bot=false},_} <- midict:to_list(Players)],
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

parent_send_game_res({ParentMod, ParentPid}, TableId, ScoringState, RoundScores, TotalScores) ->
    ParentMod:table_message(ParentPid, TableId, {game_finished, ScoringState, RoundScores, TotalScores}).

parent_send_player_connected({ParentMod, ParentPid}, TableId, PlayerId) ->
    ParentMod:table_message(ParentPid, TableId, {player_connected, PlayerId}).

parent_send_player_disconnected({ParentMod, ParentPid}, TableId, PlayerId) ->
    ParentMod:table_message(ParentPid, TableId, {player_disconnected, PlayerId}).

desk_player_action(Desk, SeatNum, Action) ->
    ?DESK:player_action(Desk, SeatNum, Action).

%%===================================================================

create_okey_game_info(#okey_state{table_name = TName, mult_factor = MulFactor,
                             slang_flag = SlangFlag, observer_flag = ObserverFlag,
                             speed = Speed, turn_timeout = TurnTimeout,
                             reveal_confirmation_timeout = RevealConfirmationTimeout,
                             ready_timeout = ReadyTimeout, game_mode = GameMode,
                             rounds = Rounds1, players = Players, tour = Tour,
                             tours = Tours, gosterge_finish_allowed = GostergeFinish,
                             tournament_type = TournamentType, pause_mode = PauseMode,
                             social_actions_enabled = SocialActionsEnabled,
                             next_series_confirmation = ConfirmMode}) ->
    PInfos = [case find_players_by_seat_num(SeatNum, Players) of
                  [#player{info = UserInfo}] -> UserInfo;
                  [] -> null
              end || SeatNum <- lists:seq(1, ?SEATS_NUM)],
    Timeouts = #'OkeyTimeouts'{speed = Speed,
                               turn_timeout = TurnTimeout,
                               challenge_timeout = RevealConfirmationTimeout,
                               ready_timeout = ReadyTimeout,
                               rematch_timeout = ?REMATCH_TIMEOUT},
    Sets = if Tours == undefined -> null; true -> Tours end,
    SetNo = if Tour == undefined -> null; true -> Tour end,
    Rounds = if Rounds1 == infinity -> -1; true -> Rounds1 end,
    #okey_game_info{table_name = list_to_binary(TName),
                    players = PInfos,
                    timeouts = Timeouts,
                    game_type = GameMode,
                    finish_with_gosterge = GostergeFinish,
                    rounds = Rounds,
                    sets = Sets,
                    set_no = SetNo,
                    mul_factor = MulFactor,
                    slang_flag = SlangFlag,
                    observer_flag = ObserverFlag,
                    pause_enabled = PauseMode == normal,
                    social_actions_enabled = SocialActionsEnabled,
                    tournament_type = TournamentType,
                    series_confirmation_mode = list_to_binary(atom_to_list(ConfirmMode))
                   }.


create_okey_game_player_state(_PlayerId, ?STATE_WAITING_FOR_START,
                              #okey_state{cur_round = CurRound, scoring_state = ScoringState,
                                     set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    Chanak = ?SCORING:chanak(ScoringState),
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #okey_game_player_state{whos_move = null,
                            game_state = game_initializing,
                            piles = null,
                            tiles = null,
                            gosterge = null,
                            pile_height = null,
                            current_round = CurRound,
                            next_turn_in = 0,
                            paused = false,
                            chanak_points = Chanak,
                            round_timeout = null,
                            set_timeout = SetTimeout};

create_okey_game_player_state(PlayerId, ?STATE_PLAYING,
                              #okey_state{timeout_timer = TRef, cur_round = CurRound,
                                     players = Players, desk_state = DeskState,
                                     scoring_state = ScoringState, round_timer = RoundTRef,
                                     round_timeout = RoundTimeout1, set_timer = SetTRef,
                                     set_timeout = SetTimeout1}) ->
    #player{seat_num = SeatNum} = fetch_player(PlayerId, Players),
    #desk_state{state = DeskStateName,
                hands = Hands,
                discarded = Discarded,
                gosterge = Gosterge,
                deck = DeskDeck,
                okey = Okey,
                cur_seat = CurSeatNum} = DeskState,
    {_, PlayerHand} = lists:keyfind(SeatNum, 1, Hands),
    Hand = [tash_to_ext(Tash,Okey) || Tash <- PlayerHand],
    #player{user_id = CurUserId} = get_player_by_seat_num(CurSeatNum, Players),
    Timeout = calc_timeout(TRef),
    F = fun(_, N) ->
                #player{user_id = UserId} = get_player_by_seat_num(N, Players),
                {_, Tahes} = lists:keyfind(N, 1, Discarded),
                {{UserId, [tash_to_ext(Tash,Okey) || Tash <- Tahes]}, next_seat_num(N)}
        end,
    {Piles, _} = lists:mapfoldl(F, prev_seat_num(SeatNum), lists:seq(1, ?SEATS_NUM)),
    GameState = statename_to_api_string(DeskStateName),
    Chanak = ?SCORING:chanak(ScoringState),
    RoundTimeout = if RoundTimeout1 == infinity -> null;
                      true -> calc_timeout_comp(RoundTRef, 2000)
                   end,
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #okey_game_player_state{whos_move = CurUserId,
                            game_state = GameState,
                            piles = Piles,
                            tiles = Hand,
                            gosterge = tash_to_ext(Gosterge),
                            pile_height = length(DeskDeck),
                            current_round = CurRound,
                            next_turn_in = Timeout,
                            paused = false,
                            chanak_points = Chanak,
                            round_timeout = RoundTimeout,
                            set_timeout = SetTimeout};

create_okey_game_player_state(PlayerId, ?STATE_REVEAL_CONFIRMATION,
                              #okey_state{timeout_timer = TRef, cur_round = CurRound,
                                     players = Players, desk_state = DeskState,
                                     scoring_state = ScoringState,
                                     set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    #player{seat_num = SeatNum} = fetch_player(PlayerId, Players),
    #desk_state{hands = Hands,
                discarded = Discarded,
                gosterge = Gosterge,
                deck = DeskDeck,
                okey = Okey,
                cur_seat = CurSeatNum} = DeskState,
    {_, PlayerHand} = lists:keyfind(SeatNum, 1, Hands),
    Hand = [tash_to_ext(Tash,Okey) || Tash <- PlayerHand],
    #player{user_id = CurUserId} = get_player_by_seat_num(CurSeatNum, Players),
    Timeout = calc_timeout(TRef),
    F = fun(_, N) ->
                Pile = case lists:keyfind(N, 1, Discarded) of
                           {_, []} -> null;
                           {_, [Tash|_]} -> tash_to_ext(Tash,Okey)
                       end,
                {Pile, next_seat_num(N)}
        end,
    {Piles, _} = lists:mapfoldl(F, prev_seat_num(SeatNum), lists:seq(1, ?SEATS_NUM)),
    Chanak = ?SCORING:chanak(ScoringState),
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #okey_game_player_state{whos_move = CurUserId,
                            game_state = do_okey_challenge,
                            piles = Piles,
                            tiles = Hand,
                            gosterge = tash_to_ext(Gosterge),
                            pile_height = length(DeskDeck),
                            current_round = CurRound,
                            next_turn_in = Timeout,
                            paused = false,
                            chanak_points = Chanak,
                            round_timeout = null,
                            set_timeout = SetTimeout};

create_okey_game_player_state(_PlayerId, ?STATE_FINISHED,
                              #okey_state{cur_round = CurRound, scoring_state = ScoringState,
                                     set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    Chanak = ?SCORING:chanak(ScoringState),
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #okey_game_player_state{whos_move = null,
                            game_state = game_initializing,
                            piles = null,
                            tiles = null,
                            gosterge = null,
                            pile_height = null,
                            current_round = CurRound,
                            next_turn_in = 0,
                            paused = false,
                            chanak_points = Chanak,
                            round_timeout = null,
                            set_timeout = SetTimeout};

create_okey_game_player_state(PlayerId, ?STATE_PAUSE,
                              #okey_state{paused_statename = PausedStateName,
                                     paused_timeout_value = Timeout
                                     } = StateData) ->
    Msg = create_okey_game_player_state(PlayerId, PausedStateName, StateData),
    Msg#okey_game_player_state{next_turn_in = Timeout,
                               paused = true}.


create_okey_game_started(SeatNum, DeskState, CurRound,
                         #okey_state{scoring_state = ScoringState, round_timeout = RoundTimeout1,
                                set_timeout = SetTimeout1, set_timer = SetTRef}) ->
    Chanak = ?SCORING:chanak(ScoringState),
    #desk_state{hands = Hands,
                gosterge = Gosterge,
                okey = Okey,
                deck = DeskDeck} = DeskState,
    {_, PlayerHand} = lists:keyfind(SeatNum, 1, Hands),
    Hand = [tash_to_ext(Tash,Okey) || Tash <- PlayerHand],
    RoundTimeout = if RoundTimeout1 == infinity -> null;
                      true -> RoundTimeout1 - 2000
                   end,
    SetTimeout = if SetTimeout1 == infinity -> null;
                    true -> calc_timeout_comp(SetTRef, 2000)
                 end,
    #okey_game_started{tiles = Hand,
                       gosterge = tash_to_ext(Gosterge),
                       pile_height = length(DeskDeck),
                       current_round = CurRound,
                       current_set = 1,        %% XXX Concept of sets is deprecated
                       chanak_points = Chanak,
                       round_timeout = RoundTimeout,
                       set_timeout = SetTimeout}.


create_okey_next_turn(CurSeat, Players, OkeyEnabled) ->
    #player{user_id = UserId} = get_player_by_seat_num(CurSeat, Players),
    #okey_next_turn{player = UserId, okey_button = OkeyEnabled}.


create_player_left(SeatNum, UserInfo, Players) ->
    #player{user_id = OldUserId} = get_player_by_seat_num(SeatNum, Players),
    IsBot = UserInfo#'PlayerInfo'.robot,
    #player_left{player = OldUserId,
                 human_replaced = not IsBot,
                 replacement = UserInfo}.


create_okey_player_has_gosterge(SeatNum, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_player_has_gosterge{player = UserId}.

create_okey_player_has_8_tashes(SeatNum, Value, Players) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_player_has_8_tashes{player = UserId,
                              value = Value}.

create_okey_enabled(SeatNum, CurSeatNum, Players) ->
    #player{user_id = Who} = get_player_by_seat_num(SeatNum, Players),
    #player{user_id = Whom} = get_player_by_seat_num(CurSeatNum, Players),
    #okey_enable{player = Whom,
                 enabled = true,
                 who = Who}.

create_okey_disable_okey(SeatNum, CurSeatNum, Players) ->
    #player{user_id = Who} = get_player_by_seat_num(SeatNum, Players),
    #player{user_id = Whom} = get_player_by_seat_num(CurSeatNum, Players),
    #okey_enable{player = Whom,
                 enabled = false,
                 who = Who}.

create_okey_tile_taken_discarded(SeatNum, Tash, PileHeight, Players, Okey) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_tile_taken{player = UserId,
                     pile = 1, %% From discarded tashes of the previous player
                     revealed = tash_to_ext(Tash, Okey),
                     pile_height = PileHeight}.


create_okey_tile_taken_table(CSN, SeatNum, Tash, PileHeight, Players, Okey) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_tile_taken{player = UserId,
                     pile = 0, %% From the deck on the table
                     revealed = case CSN == SeatNum of true -> tash_to_ext(Tash, Okey); _ -> null end,
                     pile_height = PileHeight}.


create_okey_tile_discarded(SeatNum, Tash, Timeouted, Players, Okey) ->
    #player{user_id = UserId} = get_player_by_seat_num(SeatNum, Players),
    #okey_tile_discarded{player = UserId,
                         tile = tash_to_ext(Tash, Okey),
                         timeouted = Timeouted}.

% OKEY GAME RESULTS

round_results(
    Reason,
    Revealer, RevealerWin, WrongRejects, RoundScore,
    TotalScore, PlayersAchsPoints,
    State=#okey_state{
        tournament_type=GameKind,
        game_mode=GameMode,
        speed=Speed,
        cur_round=Round,
        rounds=Rounds,
        game_id=GameId,
        players=Players}) ->

    wf:info(?MODULE,"Score ~p/~p",[RoundScore,TotalScore]),

    {Date,Time} = calendar:local_time(),

    Results = [begin

        Player = #player{user_id = UserId,is_bot=IsBot} = get_player_by_seat_num(SeatNum, Players),
        IsWinner = if SeatNum == Revealer -> RevealerWin; true -> not RevealerWin end,
        GoodShot = if SeatNum == Revealer -> RevealerWin; true -> not lists:member(SeatNum, WrongRejects) end,
        {_, PlayerScoreTotal} = lists:keyfind(SeatNum, 1, TotalScore),
        {_, PlayerScoreRound} = lists:keyfind(SeatNum, 1, RoundScore),

        RE = #reveal_event{
            id = ?GAME_STATS:timestamp(),
            feed_id = UserId,
            user = UserId,
            module = GameKind,
            speed = Speed,
            rounds = Rounds,
            date = Date,
            time = Time,
            type = GameMode,
            reason = Reason,
            winner = IsWinner,
            score = PlayerScoreRound,
            total = PlayerScoreTotal},

        PlayerInfo = Player#player.info,
        Name = PlayerInfo#'PlayerInfo'.name,
        Surname = PlayerInfo#'PlayerInfo'.surname,
        DisplayName = <<Name/binary,32,Surname/binary>>,

        case {SeatNum == Revealer,Revealer,IsBot} of
            {_,none,_} -> ?GAME_STATS:reveal_event(UserId,RE,State);
            {true,_,false}  -> ?GAME_STATS:reveal_event(UserId,RE,State);
            _ -> skip end,

        {DisplayName,IsWinner,js_hack(PlayerScoreRound),js_hack(PlayerScoreTotal)}

    end || SeatNum <- lists:seq(1, ?SEATS_NUM)],

    #okey_round_ended{
        round = Round,
        reason = Reason,
        results = Results,
        next_action = next_round}.

%js_hack(Score) when Score < 0 -> -Score * 1000000;
js_hack(Score) -> integer_to_binary(Score).

create_okey_series_ended(Results, Players, Confirm,
    #okey_state{tournament_type=GameKind,game_mode=GameMode,speed=Speed,rounds=Rounds}=GameState) ->
    {Date,Time} = calendar:local_time(),
    [begin
        #player{user_id = UserId,is_bot=IsBot} = fetch_player(PlayerId, Players),
        case IsBot of
            false ->
                Event = #series_event{result=Status,user=UserId,date=Date,time=Time,score=Score,
                    speed=Speed,rounds=Rounds,feed_id={GameMode,Speed,Rounds,UserId},
                    id=?GAME_STATS:timestamp()},
                ?GAME_STATS:series_event(UserId,Event,GameState);
            _ -> skip end
    end || {PlayerId, Position, Score, Status} <- Results],
    #okey_series_ended{standings = Results}.

create_okey_tour_result(TurnNum, Results) ->
    Records = [begin
                   #okey_turn_record{player_id = UserId, place = Position, score = Score,
                                     status = Status}
               end || {UserId, Position, Score, Status} <- Results],
    #okey_turn_result{turn_num = TurnNum,
                      records = Records}.

create_okey_revealed(SeatNum, DiscardedTash, TashPlaces, Players, Okey) ->
    #player{user_id = UserId, info = Player} = get_player_by_seat_num(SeatNum, Players),
    TashPlacesExt = [[case T of
                         null -> null;
                         _ -> tash_to_ext(T,Okey)
                      end || T <- Row ] || Row <- TashPlaces],
    Name = Player#'PlayerInfo'.name,
    Surname = Player#'PlayerInfo'.surname,
    #okey_revealed{player = <<Name/binary,32,Surname/binary>>,
                   discarded = tash_to_ext(DiscardedTash, Okey),
                   hand = TashPlacesExt}.


create_okey_turn_timeout(UserId, null, TashDiscarded, Okey) ->
    #okey_turn_timeout{player = UserId,
                       tile_taken = null,
                       tile_discarded = tash_to_ext(TashDiscarded, Okey)};
create_okey_turn_timeout(UserId, TashTaken, TashDiscarded, Okey) ->
    #okey_turn_timeout{player = UserId,
                       tile_taken = tash_to_ext(TashTaken, Okey),
                       tile_discarded = tash_to_ext(TashDiscarded, Okey)}.

create_game_paused_pause(UserId, GameId) ->
    #game_paused{game = GameId,
                 who = UserId,
                 action = pause,
                 retries = 0}.

create_game_paused_resume(UserId, GameId) ->
    #game_paused{game = GameId,
                 who = UserId,
                 action = resume,
                 retries = 0}.

create_okey_playing_tables(Num) ->
    #okey_playing_tables{num = Num}.

tash_to_ext(false_okey) -> #'OkeyPiece'{color = 2, value = 0};
tash_to_ext({Color, Value}) ->  #'OkeyPiece'{color = Color, value = Value}.

tash_to_ext(false_okey,{Color,Value}) -> #'OkeyPiece'{color = Color, value = 0};
tash_to_ext(okey,{Color,Value}) -> #'OkeyPiece'{color = Color, value = Value};
tash_to_ext({Color, Value},_) ->  #'OkeyPiece'{color = Color, value = Value}.

ext_to_tash(#'OkeyPiece'{value = 0}) -> false_okey;
ext_to_tash(#'OkeyPiece'{color = Color, value = Value}) -> {Color, Value}.

%statename_to_api_string(state_wait) -> do_okey_ready;
statename_to_api_string(state_take) ->      do_okey_take;
statename_to_api_string(state_discard) ->   do_okey_discard;
statename_to_api_string(state_finished) ->  game_finished.

desk_error_to_ext(action_disabled) -> false;
desk_error_to_ext(no_gosterge) -> false;
desk_error_to_ext(no_8_tashes) -> false;
desk_error_to_ext(no_okey_discarded) -> {error, there_is_no_okey_there};
desk_error_to_ext(not_your_order) -> {error, not_your_turn};
desk_error_to_ext(blocked) -> {error, okey_is_blocked};
desk_error_to_ext(no_tash) -> {error, no_tash};
desk_error_to_ext(no_such_tash) -> {error, no_such_tash};
desk_error_to_ext(hand_not_match) -> {error, discarded_hand_does_not_match_server_state};
desk_error_to_ext(E) -> {error, E}.

%%===================================================================

get_timeout(turn, fast) -> {ok, Val}   = kvs:get(config,"games/okey/turn_timeout_fast", 15000), Val;
get_timeout(turn, normal) -> {ok, Val} = kvs:get(config,"games/okey/turn_timeout_normal", 30000), Val;
get_timeout(turn, slow) -> {ok, Val}   = kvs:get(config,"games/okey/turn_timeout_slow", 60000), Val;

get_timeout(challenge, fast) ->  {ok, Val}   = kvs:get(config,"games/okey/challenge_timeout_fast", 5000), Val;
get_timeout(challenge, normal) ->  {ok, Val} = kvs:get(config,"games/okey/challenge_timeout_normal", 10000), Val;
get_timeout(challenge, slow) -> {ok, Val}    = kvs:get(config,"games/okey/challenge_timeout_slow", 20000), Val;

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

next_seat_num(?SEATS_NUM) -> 1;
next_seat_num(N) -> N + 1.

prev_seat_num(1) -> ?SEATS_NUM;
prev_seat_num(N) -> N - 1.

%%===================================================================
init_desk_state(Desk) ->
    SeatsNums = ?DESK:get_seats_nums(Desk),
    Hands = [{SeatNum, ?DESK:get_hand(Desk, SeatNum)} || SeatNum <- SeatsNums],
    Discarded = [{SeatNum, ?DESK:get_discarded(Desk, SeatNum)} || SeatNum <- SeatsNums],
    #desk_state{state = ?DESK:get_state_name(Desk),
                hands = Hands,
                discarded = Discarded,
                deck = ?DESK:get_deck(Desk),
                cur_seat = ?DESK:get_cur_seat(Desk),
                gosterge = ?DESK:get_gosterge(Desk),
                okey = ?DESK:gosterge_to_okey(?DESK:get_gosterge(Desk)),
                have_8_tashes = ?DESK:get_have_8_tashes(Desk),
                has_gosterge = ?DESK:get_has_gosterge(Desk)}.

%%===================================================================

choose_gosterge(Deck) ->
    Pos = crypto:rand_uniform(1, deck:size(Deck)),
    case deck:get(Pos, Deck) of
        {false_okey, _} -> choose_gosterge(Deck);
        {Gosterge, Deck1} -> {Gosterge, Deck1}
    end.
