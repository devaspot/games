%%% -------------------------------------------------------------------
%%% Author  : Sergii Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The "Eliminate tournament" logic
%%%
%%% Created : Nov 02, 2012
%%% -------------------------------------------------------------------

%%% Terms explanation:
%%% GameId   - uniq identifier of the tournament. Type: integer().
%%% PlayerId - registration number of a player in the tournament. Type: integer()
%%% UserId   - cross system identifier of a physical user. Type: binary() (or string()?).
%%% TableId  - uniq identifier of a table in the tournament. Used by the
%%%          tournament logic. Type: integer().
%%% TableGlobalId - uniq identifier of a table in the system. Can be used
%%%          to refer to a table directly - without pointing to a tournament.
%%%          Type: integer()

-module(elimination).

-behaviour(gen_fsm).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("server/include/basic_types.hrl").
-include_lib("db/include/table.hrl").
-include_lib("db/include/transaction.hrl").
-include_lib("db/include/scoring.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1, start/2, start_link/2, reg/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([table_message/3, client_message/2, client_request/2, client_request/3,
         system_request/2, system_request/3]).

-record(state,
        {%% Static values
         game_id           :: pos_integer(),
         trn_id            :: term(),
         table_params      :: proplists:proplist(),
         tours_plan        :: list(integer()), %% Defines how many players will be passed to a next tour
         tours             :: integer(),
         players_per_table :: integer(),
         quota_per_round   :: integer(), %% Using for the calculation of qouta amount which be deducted from players every tour
         rounds_per_tour   :: integer(), %% Using for the calculation of qouta amount which be deducted from players every tour
         speed             :: fast | normal,
         awards            :: list(), %% [FirtsPrize, SecondPrize, ThirdPrize], Prize = undefined | GiftId
         demo_mode         :: boolean(), %% If true then results of tours will be generated randomly
         table_module      :: atom(),
         game_type         :: atom(), %% Using only for quota transactions
         game_mode         :: atom(), %% Using only for quota transactions
         %% Dinamic values
         players,          %% The register of tournament players
         tables,           %% The register of tournament tables
         seats,            %% Stores relation between players and tables seats
         tournament_table  :: list(), %% [{TurnNum, TurnRes}], TurnRes = [{PlayerId, CommonPos, Points, Status}]
         table_id_counter  :: pos_integer(),
         tour              :: pos_integer(),
         cr_tab_requests   :: dict(),  %% {TableId, PlayersIds}
         reg_requests      :: dict(),  %% {PlayerId, From}
         tab_requests      :: dict(),  %% {RequestId, RequestContext}
         timer             :: undefined | reference(),
         timer_magic       :: undefined | reference(),
         tables_wl         :: list(), %% Tables waiting list
         tables_results    :: list()  %% [{TableId, TableResult}]
        }).

-record(player,
        {
         id              :: pos_integer(),
         user_id,
         user_info       :: #'PlayerInfo'{},
         is_bot          :: boolean(),
         status          :: active | eliminated
        }).

-record(table,
        {
         id              :: pos_integer(),
         global_id       :: pos_integer(),
         pid             :: pid(),
         relay           :: {atom(), pid()}, %% {RelayMod, RelayPid}
         mon_ref         :: reference(),
         state           :: initializing | ready | in_process | finished,
         context         :: term(), %% Context term of a table. For failover proposes.
         timer           :: reference()
        }).

-record(seat,
        {
         table           :: pos_integer(),
         seat_num        :: integer(),
         player_id       :: undefined | pos_integer(),
         registered_by_table :: undefined | boolean(),
         connected       :: undefined | boolean()
        }).


-define(STATE_INIT, state_init).
-define(STATE_WAITING_FOR_TABLES, state_waiting_for_tables).
-define(STATE_WAITING_FOR_PLAYERS, state_waiting_for_players).
-define(STATE_TURN_PROCESSING, state_turn_processing).
-define(STATE_SHOW_SERIES_RESULT, state_show_series_result).
-define(STATE_FINISHED, state_finished).

-define(TABLE_STATE_INITIALIZING, initializing).
-define(TABLE_STATE_READY, ready).
-define(TABLE_STATE_IN_PROGRESS, in_progress).
-define(TABLE_STATE_WAITING_NEW_ROUND, waiting_new_round).
-define(TABLE_STATE_FINISHED, finished).

-define(WAITING_PLAYERS_TIMEOUT, 3000). %% Time between all table was created and starting a turn
-define(REST_TIMEOUT, 5000).            %% Time between a round finish and start of a new one
-define(SHOW_SERIES_RESULT_TIMEOUT, 15000). %% Time between a tour finish and start of a new one
-define(SHOW_TOURNAMENT_RESULT_TIMEOUT, 15000). %% Time between last tour result showing and the tournament finish

-define(TOURNAMENT_TYPE, elimination).
-define(ROUNDS_PER_TOUR, 10).
%% ====================================================================
%% External functions
%% ====================================================================

start([GameId, Params]) -> start(GameId, Params).

start(GameId, Params) ->
    gen_fsm:start(?MODULE, [GameId, Params, self()], []).

start_link(GameId, Params) ->
    gen_fsm:start_link(?MODULE, [GameId, Params, self()], []).

reg(Pid, User) ->
    client_request(Pid, {join, User}, 10000).

table_message(Pid, TableId, Message) ->
    gen_fsm:send_all_state_event(Pid, {table_message, TableId, Message}).

client_message(Pid, Message) ->
    gen_fsm:send_all_state_event(Pid, {client_message, Message}).

client_request(Pid, Message) ->
    client_request(Pid, Message, 5000).

client_request(Pid, Message, Timeout) ->
    gen_fsm:sync_send_all_state_event(Pid, {client_request, Message}, Timeout).

system_request(Pid, Message) ->
    system_request(Pid, Message, 5000).

system_request(Pid, Message, Timeout) ->
    gen_fsm:sync_send_all_state_event(Pid, {system_request, Message}, Timeout).

%% ====================================================================
%% Server functions
%% ====================================================================

init([GameId, Params, _Manager]) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Init started",[GameId]),
    Registrants = get_param(registrants, Params),
    QuotaPerRound = get_param(quota_per_round, Params),
    RoundsPerTour = get_param(rounds_per_tour, Params),
    Tours = get_param(tours, Params),
    ToursPlan = get_param(plan, Params),
    PlayersPerTable = get_param(players_per_table, Params),
    Speed = get_param(speed, Params),
    GameType = get_param(game_type, Params),
    GameMode = get_param(game_mode, Params),
    Awards = get_param(awards, Params),
    TableParams = get_param(table_params, Params),
    TableModule = get_param(table_module, Params),

    DemoMode = get_option(demo_mode, Params, false),
    TrnId = get_option(trn_id, Params, undefined),

    [gas:info(?MODULE,"TRN_ELIMINATION_DBG <~p> Parameter <~p> : ~p", [GameId, P, V]) ||
     {P, V} <- Params],

    Players = setup_players(Registrants),
    PlayersIds = get_players_ids(Players),
    TTable = ttable_init(PlayersIds),

    gas:info(?MODULE,"TRN_ELIMINATION_DBG <~p> TTable: ~p", [GameId, TTable]),
    gas:info(?MODULE,"TRN_ELIMINATION <~p> started.  Pid:~p", [GameId, self()]),

    gen_fsm:send_all_state_event(self(), go),
    {ok, ?STATE_INIT, #state{game_id = GameId,
                             trn_id = TrnId,
                             table_params = TableParams,
                             quota_per_round = QuotaPerRound,
                             rounds_per_tour = RoundsPerTour,
                             tours_plan = ToursPlan,
                             tours = Tours,
                             players_per_table = PlayersPerTable,
                             speed = Speed,
                             awards  = Awards,
                             demo_mode = DemoMode,
                             table_module = TableModule,
                             game_type = GameType,
                             game_mode = GameMode,
                             players = Players,
                             tournament_table = TTable,
                             table_id_counter = 1
                            }}.

%%===================================================================
handle_event(go, ?STATE_INIT, #state{game_id = GameId, trn_id = TrnId,
                                     game_type = GameType} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Received a directive to starting the tournament.", [GameId]),
    GProcVal = #game_table{game_type = GameType,
                           game_process = self(),
                           game_module = ?MODULE,
                           id = GameId,
                           trn_id = TrnId,
                           age_limit = 100,
                           game_mode = undefined,
                           game_speed = undefined,
                           feel_lucky = false,
                           owner = undefined,
                           creator = undefined,
                           rounds = undefined,
                           pointing_rules   = [],
                           pointing_rules_ex = [],
                           users = [],
                           name = "Elimination Tournament - " ++ erlang:integer_to_list(GameId) ++ " "
                          },
    gproc:reg({p,l,self()}, GProcVal),
    init_tour(1, StateData);

handle_event({client_message, Message}, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Received the message from a client: ~p.", [GameId, Message]),
    handle_client_message(Message, StateName, StateData);

handle_event({table_message, TableId, Message}, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Received the message from table <~p>: ~p.", [GameId, TableId, Message]),
    handle_table_message(TableId, Message, StateName, StateData);

handle_event(Message, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled message(event) received in state <~p>: ~p.",
          [GameId, StateName, Message]),
    {next_state, StateName, StateData}.

handle_sync_event({client_request, Request}, From, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Received the request from a client: ~p.", [GameId, Request]),
    handle_client_request(Request, From, StateName, StateData);

handle_sync_event({system_request, Request}, From, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Received the request from the system: ~p.", [GameId, Request]),
    handle_system_request(Request, From, StateName, StateData);

handle_sync_event(Request, From, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled request(event) received in state <~p> from ~p: ~p.",
          [GameId, StateName, From, Request]),
    {reply, {error, unknown_request}, StateName, StateData}.

%%===================================================================

handle_info({'DOWN', MonRef, process, _Pid, _}, StateName,
            #state{game_id = GameId, tables = Tables} = StateData) ->
    case get_table_by_mon_ref(MonRef, Tables) of
        #table{id = TableId} ->
            gas:info(?MODULE,"TRN_ELIMINATION <~p> Table <~p> is down. Stopping", [GameId, TableId]),
            %% TODO: More smart handling (failover) needed
            {stop, {one_of_tables_down, TableId}, StateData};
        not_found ->
            {next_state, StateName, StateData}
    end;


handle_info({rest_timeout, TableId}, StateName,
            #state{game_id = GameId, tables = Tables, table_module = TableModule} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Time to start new round for table <~p>.", [GameId, TableId]),
    #table{pid = TablePid, state = TableState} = Table = fetch_table(TableId, Tables),
    if TableState == ?TABLE_STATE_WAITING_NEW_ROUND ->
           NewTable = Table#table{state = ?TABLE_STATE_IN_PROGRESS},
           NewTables = store_table(NewTable, Tables),
           send_to_table(TableModule, TablePid, start_round),
           {next_state, StateName, StateData#state{tables = NewTables}};
       true ->
           gas:info(?MODULE,"TRN_ELIMINATION <~p> Don't start new round at table <~p> because it is not waiting for start.",
                 [GameId, TableId]),
           {next_state, StateName, StateData}
    end;


handle_info({timeout, Magic}, ?STATE_WAITING_FOR_PLAYERS,
            #state{timer_magic = Magic, game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Time to start new turn.", [GameId]),
    start_turn(StateData);


handle_info({timeout, Magic}, ?STATE_SHOW_SERIES_RESULT,
            #state{timer_magic = Magic, tour = Tour, tours = Tours,
                   game_id = GameId} = StateData) ->
    if Tour == Tours ->
           gas:info(?MODULE,"TRN_ELIMINATION <~p> Time to finalize the tournament.", [GameId]),
           finalize_tournament(StateData);
       true ->
           NewTour = Tour + 1,
           gas:info(?MODULE,"TRN_ELIMINATION <~p> Time to initialize tour <~p>.", [GameId, NewTour]),
           init_tour(NewTour, StateData)
    end;


handle_info({timeout, Magic}, ?STATE_FINISHED,
            #state{timer_magic = Magic, tables = Tables, game_id = GameId,
                   table_module = TableModule} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Time to stopping the tournament.", [GameId]),
    finalize_tables_with_disconnect(TableModule, Tables),
    {stop, normal, StateData#state{tables = [], seats = []}};


handle_info(Message, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled message(info) received in state <~p>: ~p.",
          [GameId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================

terminate(_Reason, _StateName, #state{game_id=GameId}=_StatData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Shutting down at state: <~p>. Reason: ~p",
          [GameId, _StateName, _Reason]),
    ok.

%%===================================================================

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------


handle_client_message(Message, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled client message received in "
          "state <~p>: ~p.", [GameId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================

handle_table_message(TableId, {player_connected, PlayerId},
                     StateName,
                     #state{seats = Seats} = StateData) ->
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, true, Seats),
            {next_state, StateName, StateData#state{seats = NewSeats}};
        [] -> %% Ignoring the message
            {next_state, StateName, StateData}
    end;


handle_table_message(TableId, {player_disconnected, PlayerId},
                     StateName, #state{seats = Seats} = StateData) ->
    case find_seats_by_player_id(PlayerId, Seats) of
        [#seat{seat_num = SeatNum}] ->
            NewSeats = update_seat_connect_status(TableId, SeatNum, false, Seats),
            {next_state, StateName, StateData#state{seats = NewSeats}};
        [] -> %% Ignoring the message
            {next_state, StateName, StateData}
    end;


handle_table_message(TableId, {table_created, Relay},
                     ?STATE_WAITING_FOR_TABLES,
                     #state{tables = Tables, seats = Seats, cr_tab_requests = TCrRequests,
                            table_module = TableModule, reg_requests = RegRequests} = StateData) ->
    TabInitPlayers = dict:fetch(TableId, TCrRequests),
    NewTCrRequests = dict:erase(TableId, TCrRequests),
    %% Update status of players
    TabSeats = find_seats_by_table_id(TableId, Seats),
    F = fun(#seat{player_id = PlayerId} = S, Acc) ->
                case lists:member(PlayerId, TabInitPlayers) of
                    true -> store_seat(S#seat{registered_by_table = true}, Acc);
                    false -> Acc
                end
        end,
    NewSeats = lists:foldl(F, Seats, TabSeats),

    %% Process delayed registration requests
    TablePid = get_table_pid(TableId, Tables),
    F2 = fun(PlayerId, Acc) ->
                 case dict:find(PlayerId, Acc) of
                     {ok, From} ->
                         gen_fsm:reply(From, {ok, {PlayerId, Relay, {TableModule, TablePid}}}),
                         dict:erase(PlayerId, Acc);
                     error -> Acc
                 end
         end,
    NewRegRequests = lists:foldl(F2, RegRequests, TabInitPlayers),
    NewTables = update_created_table(TableId, Relay, Tables),
    case dict:size(NewTCrRequests) of
        0 -> 
            {TRef, Magic} = start_timer(?WAITING_PLAYERS_TIMEOUT),
            {next_state, ?STATE_WAITING_FOR_PLAYERS,
              StateData#state{tables = NewTables, seats = NewSeats, cr_tab_requests = NewTCrRequests,
                              reg_requests = NewRegRequests, timer = TRef, timer_magic = Magic}};
        _ -> {next_state, ?STATE_WAITING_FOR_TABLES,
              StateData#state{tables = NewTables, seats = NewSeats,
                              cr_tab_requests = NewTCrRequests, reg_requests = NewRegRequests}}
    end;


handle_table_message(TableId, {round_finished, NewScoringState, _RoundScore, _TotalScore},
                     ?STATE_TURN_PROCESSING,
                     #state{tables = Tables, table_module = TableModule} = StateData) ->
    #table{pid = TablePid} = Table = fetch_table(TableId, Tables),
    TRef = erlang:send_after(?REST_TIMEOUT, self(), {rest_timeout, TableId}),
    NewTable = Table#table{context = NewScoringState, state = ?TABLE_STATE_WAITING_NEW_ROUND, timer = TRef},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TableModule, TablePid, show_round_result),
    {next_state, ?STATE_TURN_PROCESSING, StateData#state{tables = NewTables}};


handle_table_message(TableId, {game_finished, TableContext, _RoundScore, TotalScore},
                     ?STATE_TURN_PROCESSING = StateName,
                     #state{tables = Tables, tables_wl = WL, demo_mode = DemoMode,
                            tables_results = TablesResults, table_module = TableModule
                           } = StateData) ->
    TableScore = if DemoMode -> [{PlayerId, crypto:rand_uniform(1, 30)} || {PlayerId, _} <- TotalScore];
                    true -> TotalScore
                 end,
    NewTablesResults = [{TableId, TableScore} | TablesResults],
    #table{pid = TablePid, state = TableState, timer = TRef} = Table = fetch_table(TableId, Tables),
    if TableState == ?TABLE_STATE_WAITING_NEW_ROUND -> erlang:cancel_timer(TRef);
       true -> do_nothing
    end,
    NewTable = Table#table{context = TableContext, state = ?TABLE_STATE_FINISHED, timer = undefined},
    NewTables = store_table(NewTable, Tables),
    send_to_table(TableModule, TablePid, show_round_result),
    NewWL = lists:delete(TableId, WL),
    [send_to_table(TableModule, TPid, {playing_tables_num, length(NewWL)})
       || #table{pid = TPid, state = ?TABLE_STATE_FINISHED} <- tables_to_list(Tables)],
    if NewWL == [] ->
           process_tour_result(StateData#state{tables = NewTables,
                                               tables_results = NewTablesResults,
                                               tables_wl = []});
       true ->
           {next_state, StateName, StateData#state{tables = NewTables,
                                                   tables_results = NewTablesResults,
                                                   tables_wl = NewWL}}
    end;


handle_table_message(TableId, {response, RequestId, Response},
                     StateName,
                     #state{game_id = GameId, tab_requests = TabRequests} = StateData) ->
    NewTabRequests = dict:erase(RequestId, TabRequests),
    case dict:find(RequestId, TabRequests) of
        {ok, ReqContext} ->
            gas:info(?MODULE,"TRN_ELIMINATION <~p> The a response received from table <~p>. "
                  "RequestId: ~p. Request context: ~p. Response: ~p",
                  [GameId, TableId, RequestId, ReqContext, Response]),
            handle_table_response(TableId, ReqContext, Response, StateName,
                                  StateData#state{tab_requests = NewTabRequests});
        error ->
            gas:error(?MODULE,"TRN_ELIMINATION <~p> Table <~p> sent a response for unknown request. "
                   "RequestId: ~p. Response", []),
            {next_state, StateName, StateData#state{tab_requests = NewTabRequests}}
    end;


handle_table_message(TableId, Message, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled table message received from table <~p> in "
          "state <~p>: ~p.", [GameId, TableId, StateName, Message]),
    {next_state, StateName, StateData}.

%%===================================================================
%% handle_table_response(_TableId, {register_player, PlayerId, TableId, SeatNum}, ok = _Response,
%%                       StateName,
%%                       #state{reg_requests = RegRequests, seats = Seats,
%%                              tables = Tables} = StateData) ->
%%     Seat = fetch_seat(TableId, SeatNum, Seats),
%%     NewSeats = store_seat(Seat#seat{registered_by_table = true}, Seats),
%%     %% Send response to a client for a delayed request
%%     NewRegRequests =
%%         case dict:find(PlayerId, RegRequests) of
%%             {ok, From} ->
%%                 #table{relay = Relay, pid = TablePid} = fetch_table(TableId, Tables),
%%                 gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}),
%%                 dict:erase(PlayerId, RegRequests);
%%             error -> RegRequests
%%         end,
%%     {next_state, StateName, StateData#state{seats = NewSeats,
%%                                             reg_requests = NewRegRequests}};

%% handle_table_response(_TableId, {replace_player, PlayerId, TableId, SeatNum}, ok = _Response,
%%                       StateName,
%%                       #state{reg_requests = RegRequests, seats = Seats,
%%                              tables = Tables} = StateData) ->
%%     Seat = fetch_seat(TableId, SeatNum, Seats),
%%     NewSeats = store_seat(Seat#seat{registered_by_table = true}, Seats),
%%     %% Send response to a client for a delayed request
%%     NewRegRequests =
%%         case dict:find(PlayerId, RegRequests) of
%%             {ok, From} ->
%%                 #table{relay = Relay, pid = TablePid} = fetch_table(TableId, Tables),
%%                 gen_fsm:reply(From, {ok, {PlayerId, Relay, {?TAB_MOD, TablePid}}}),
%%                 dict:erase(PlayerId, RegRequests);
%%             error -> RegRequests
%%         end,
%%     {next_state, StateName, StateData#state{seats = NewSeats,
%%                                             reg_requests = NewRegRequests}}.

handle_table_response(TableId, RequestContext, Response, StateName,
                      #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled 'table response' received from table <~p> "
          "in state <~p>. Request context: ~p. Response: ~p.",
          [GameId, TableId, StateName, RequestContext, Response]),
    {next_state, StateName, StateData}.

%%===================================================================

handle_client_request({join, User}, From, StateName,
                      #state{game_id = GameId, reg_requests = RegRequests,
                             seats = Seats, players=Players, tables = Tables,
                             table_module = TableModule} = StateData) ->
    #'PlayerInfo'{id = UserId, robot = _IsBot} = User,
    gas:info(?MODULE,"TRN_ELIMINATION <~p> The 'Join' request received from user: ~p.", [GameId, UserId]),
    if StateName == ?STATE_FINISHED ->
           gas:info(?MODULE,"TRN_ELIMINATION <~p> The tournament is finished. "
                 "Reject to join user ~p.", [GameId, UserId]),
           {reply, {error, finished}, StateName, StateData};
       true ->
           case get_player_by_user_id(UserId, Players) of
               {ok, #player{status = active, id = PlayerId}} -> %% The user is an active member of the tournament.
                   gas:info(?MODULE,"TRN_ELIMINATION <~p> User ~p is an active member of the tournament. "
                         "Allow to join.", [GameId, UserId]),
                   [#seat{table = TableId, registered_by_table = RegByTable}] = find_seats_by_player_id(PlayerId, Seats),
                   case RegByTable of
                       false -> %% Store this request to the waiting pool
                           gas:info(?MODULE,"TRN_ELIMINATION <~p> User ~p not yet regirested by the table. "
                                 "Add the request to the waiting pool.", [GameId, UserId]),
                           NewRegRequests = dict:store(PlayerId, From, RegRequests),
                           {next_state, StateName, StateData#state{reg_requests = NewRegRequests}};
                       _ ->
                           gas:info(?MODULE,"TRN_ELIMINATION <~p> Return join response for player ~p immediately.",
                                 [GameId, UserId]),
                           #table{relay = Relay, pid = TPid} = fetch_table(TableId, Tables),
                           {reply, {ok, {PlayerId, Relay, {TableModule, TPid}}}, StateName, StateData}
                   end;
               {ok, #player{status = eliminated}} ->
                   gas:info(?MODULE,"TRN_ELIMINATION <~p> User ~p is member of the tournament but he was eliminated. "
                         "Reject to join.", [GameId, UserId]),
                   {reply, {error, out}, StateName, StateData};
               error -> %% Not a member
                   gas:info(?MODULE,"TRN_ELIMINATION <~p> User ~p is not a member of the tournament. "
                         "Reject to join.", [GameId, UserId]),
                   {reply, {error, not_allowed}, StateName, StateData}
           end
    end;

handle_client_request(Request, From, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled client request received from ~p in "
          "state <~p>: ~p.", [GameId, From, StateName, Request]),
   {reply, {error, unexpected_request}, StateName, StateData}.

handle_system_request(last_tour_result, _From, StateName,
                      #state{game_id = GameId, tournament_table = TTable,
                             players = Players} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Received request for the last tour results.", [GameId]),
    {LastTourNum, TourResultsRaw} = hd(lists:reverse(lists:keysort(1, TTable))),
    TourResults = [{get_user_id(PlayerId, Players), CommonPos, Points, Status}
                   || {PlayerId, CommonPos, Points, Status} <- TourResultsRaw],
    {reply, {ok, {LastTourNum, TourResults}}, StateName, StateData};

handle_system_request(Request, From, StateName, #state{game_id = GameId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Unhandled system request received from ~p in "
          "state <~p>: ~p.", [GameId, From, StateName, Request]),
    {reply, {error, unexpected_request}, StateName, StateData}.

%%===================================================================
init_tour(Tour, #state{game_id = GameId, tours_plan = Plan, tournament_table = TTable,
                       table_params = TableParams, players = Players, quota_per_round = QuotaPerRound,
                       table_id_counter = TableIdCounter, tables = OldTables, tours = Tours,
                       players_per_table = PlayersPerTable, game_type = GameType,
                       game_mode = GameMode, table_module = TableModule,
                       rounds_per_tour = RoundsPerTour} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Initializing tour <~p>...", [GameId, Tour]),
    PlayersList = prepare_players_for_new_tour(Tour, TTable, Plan, Players),
    PrepTTable = prepare_ttable_for_tables(TTable, Players),
    UsersIds = [UserId || {_, #'PlayerInfo'{id = UserId}, _} <- PlayersList],
    deduct_quota(GameId, GameType, GameMode, QuotaPerRound * RoundsPerTour, UsersIds),
    {NewTables, Seats, NewTableIdCounter, CrRequests} =
        setup_tables(TableModule, PlayersList, PlayersPerTable, PrepTTable, Tour,
                     Tours, TableIdCounter, GameId, TableParams),
    if Tour > 1 -> finalize_tables_with_rejoin(TableModule, OldTables);
       true -> do_nothing
    end,
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Initializing of tour <~p> is finished. "
          "Waiting creating confirmations from the tours' tables...",
          [GameId, Tour]),
    {next_state, ?STATE_WAITING_FOR_TABLES, StateData#state{tables = NewTables,
                                                            seats = Seats,
                                                            table_id_counter = NewTableIdCounter,
                                                            tour = Tour,
                                                            cr_tab_requests = CrRequests,
                                                            reg_requests = dict:new(),
                                                            tab_requests = dict:new(),
                                                            tables_results = []
                                                           }}.

start_turn(#state{game_id = GameId, tour = Tour, tables = Tables,
                  table_module = TableModule} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Starting tour <~p>...", [GameId, Tour]),
    TablesList = tables_to_list(Tables),
    [send_to_table(TableModule, Pid, start_round) || #table{pid = Pid} <- TablesList],
    F = fun(Table, Acc) ->
                store_table(Table#table{state = ?TABLE_STATE_IN_PROGRESS}, Acc)
        end,
    NewTables = lists:foldl(F, Tables, TablesList),
    WL = [T#table.id || T <- TablesList],
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Tour <~p> is started. Processing...",
          [GameId, Tour]),
    {next_state, ?STATE_TURN_PROCESSING, StateData#state{tables = NewTables,
                                                         tables_wl = WL}}.


process_tour_result(#state{game_id = GameId, tournament_table = TTable, tours = Tours,
                           tours_plan = Plan, tour = Tour, tables_results = TablesResults,
                           players = Players, tables = Tables, trn_id = TrnId,
                           table_module = TableModule} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Tour <~p> is completed. Starting results processing...", [GameId, Tour]),
    TourType = lists:nth(Tour, Plan),
    TourResult1 = case TourType of
                     ne -> tour_result_all(TablesResults);
                     {te, Limit} -> tour_result_per_table(Limit, TablesResults);
                     {ce, Limit} -> tour_result_overall(Limit, TablesResults)
                 end,
    TourResult = set_tour_results_position(TourResult1), %% [{PlayerId, CommonPos, Points, Status}]
    NewTTable = ttable_store_tour_result(Tour, TourResult, TTable),
    F = fun({PlayerId, _, _, eliminated}, Acc) -> set_player_status(PlayerId, eliminated, Acc);
           (_, Acc) -> Acc
        end,
    NewPlayers = lists:foldl(F, Players, TourResult),
    TourResultWithUserId = [{get_user_id(PlayerId, Players), Position, Points, Status}
                            || {PlayerId, Position, Points, Status} <- TourResult],
    TablesResultsWithPos = set_tables_results_position(TablesResults, TourResult),
    [send_to_table(TableModule, TablePid, {tour_result, Tour, TourResultWithUserId})
       || #table{pid = TablePid} <- tables_to_list(Tables)],
    [send_to_table(TableModule, get_table_pid(TableId, Tables),
                   {show_series_result, subs_status(TableResultWithPos, Tour, Plan)})
       || {TableId, TableResultWithPos} <- TablesResultsWithPos],
    TourResultWithStrUserId = [{user_id_to_string(UserId), Position, Points, Status}
                               || {UserId, Position, Points, Status} <- TourResultWithUserId],
    wf:send(["system", "tournament_tour_note"], {TrnId, Tour, Tours, TourType, TourResultWithStrUserId}),
    {TRef, Magic} = start_timer(?SHOW_SERIES_RESULT_TIMEOUT),
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Results processing of tour <~p> is finished. "
          "Waiting some time (~p secs) before continue...",
          [GameId, Tour, ?SHOW_SERIES_RESULT_TIMEOUT div 1000]),
    {next_state, ?STATE_SHOW_SERIES_RESULT, StateData#state{timer = TRef, timer_magic = Magic,
                                                            tournament_table = NewTTable,
                                                            players = NewPlayers}}.

finalize_tournament(#state{game_id = GameId, awards = Awards, tournament_table = TTable,
                           players = Players, trn_id = TrnId} = StateData) ->
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Finalizing the tournament...", [GameId]),
    AwardsDistrib = awards_distribution(TTable, Awards),
    AwardsDistribUserId = [{user_id_to_string(get_user_id(PlayerId, Players)), Pos, GiftId}
                           || {PlayerId, Pos, GiftId} <- AwardsDistrib],
    [wf:send(["gifts", "user", UserId, "give_gift"], {GiftId})
       || {UserId, _Pos, GiftId} <- AwardsDistribUserId],
    %% TODO: Do we need advertise the prizes to game clients?
    gas:info(?MODULE,"TRN_ELIMINATION <~p> Awards distribution: ~p", [GameId, AwardsDistribUserId]),
    wf:send(["system", "tournament_ends_note"], {TrnId, AwardsDistribUserId}),
    {TRef, Magic} = start_timer(?SHOW_TOURNAMENT_RESULT_TIMEOUT),
    gas:info(?MODULE,"TRN_ELIMINATION <~p> The tournament is finalized. "
          "Waiting some time (~p secs) before continue...",
          [GameId, ?SHOW_TOURNAMENT_RESULT_TIMEOUT div 1000]),
    {next_state, ?STATE_FINISHED, StateData#state{timer = TRef, timer_magic = Magic}}.

%% awards_distribution(TTable, Awards) -> [{PlayerId, Pos, GiftId}]
awards_distribution(TTable, Awards) ->
    MTTable = merge_tournament_table(TTable),
    F = fun({PlayerId, Pos}, Acc) ->
                try lists:nth(Pos, Awards) of
                    undefined -> Acc;
                    GiftId -> [{PlayerId, Pos, GiftId} | Acc]
                catch
                    _:_ -> Acc
                end
        end,
    lists:foldl(F, [], MTTable).

%% merge_tournament_table(TTable) -> [{PlayerId, Pos}]
merge_tournament_table(TTable) ->
    [{_, Tour0} | Tours] = lists:keysort(1, TTable),
    PlayersPos0 = tour_res_to_players_pos(Tour0),
    F = fun({_, Tour}, Acc) ->
                lists:foldl(fun({PlayerId, Pos}, Acc2) ->
                                    lists:keyreplace(PlayerId, 1, Acc2, {PlayerId, Pos})
                            end, Acc, tour_res_to_players_pos(Tour))
        end,
    lists:foldl(F, PlayersPos0, Tours).

%% tour_res_to_players_pos(TourRes) -> [{PlayerId, Pos}]
tour_res_to_players_pos(TourRes) ->
    [{PlayerId, Pos} || {PlayerId, Pos, _Points, _Status} <- TourRes].


deduct_quota(GameId, GameType, GameMode, Amount, UsersIds) ->
    TI = #ti_game_event{game_name = GameType, game_mode = GameMode,
                        id = GameId, double_points = 1,
                        type = start_tour, tournament_type = ?TOURNAMENT_TYPE},
    [
%        nsm_accounts:transaction(binary_to_list(UserId), ?CURRENCY_QUOTA, -Amount, TI)
    kvs:add(#transaction{
        id=kvs:next_id(transaction,1),
        feed_id={quota,binary_to_list(UserId)},
        comment=TI})

       || UserId <- UsersIds],
    ok.


tour_result_all(TablesResults) ->
    F = fun({_, TableRes}, Acc) ->
            [{Pl, Points, active} || {Pl, Points} <- TableRes] ++ Acc
        end,
    lists:foldl(F, [], TablesResults).


tour_result_per_table(NextTourLimit, TablesResults) ->
    F = fun({_, TableResult}, Acc) ->
                SortedRes = sort_results(TableResult),
                {Winners, _} = lists:unzip(lists:sublist(SortedRes, NextTourLimit)),
                [case lists:member(Pl, Winners) of
                     true -> {Pl, Points, active};
                     false -> {Pl, Points, eliminated}
                 end || {Pl, Points} <- TableResult] ++ Acc
        end,
    lists:foldl(F, [], TablesResults).


tour_result_overall(TourLimit, TablesResults) ->
    F = fun({_, TableRes}, Acc) -> TableRes ++ Acc end,
    OverallResults = lists:foldl(F, [], TablesResults),
    SortedResults = sort_results(OverallResults),
    {Winners, _} = lists:unzip(lists:sublist(SortedResults, TourLimit)),
    [case lists:member(Pl, Winners) of
         true -> {Pl, Points, active};
         false -> {Pl, Points, eliminated}
     end || {Pl, Points} <- OverallResults].

%% set_tour_results_position([{PlayerId, Points, Status}]) -> [{PlayerId, Pos, Points, Status}]
set_tour_results_position(TourResult) ->
    F = fun({PlayerId, Points, Status}, Pos) ->
                {{PlayerId, Pos, Points, Status}, Pos + 1}
        end,
    {TourResultsWithPos, _} = lists:mapfoldl(F, 1, sort_results2(TourResult)),
    TourResultsWithPos.

%% set_tables_results_position/2 -> [{TableId, [{PlayerId, Position, Points, Status}]}]
set_tables_results_position(TablesResults, TurnResult) ->
    [begin
         TabResWithStatus = [lists:keyfind(PlayerId, 1, TurnResult) || {PlayerId, _} <- TableResult],
         {TableId, set_table_results_position(TabResWithStatus)}
     end || {TableId, TableResult} <- TablesResults].

%% set_table_results_position([{PlayerId, CommonPos, Points, Status}]) -> [{PlayerId, TabPos, Points, Status}]
set_table_results_position(TableResult) ->
    F = fun({PlayerId, _, Points, Status}, Pos) ->
                {{PlayerId, Pos, Points, Status}, Pos + 1}
        end,
    {TurnResultsWithPos, _} = lists:mapfoldl(F, 1, lists:keysort(2, TableResult)),
    TurnResultsWithPos.

subs_status(TableResultWithPos, Turn, Plan) ->
    LastTurn = Turn == length(Plan),
    {ActSubst, ElimSubst} = if LastTurn -> {winner, eliminated};
                               true -> {none, eliminated}
                            end,
    [case Status of
         active -> {PlayerId, Pos, Points, ActSubst};
         eliminated -> {PlayerId, Pos, Points, ElimSubst}
     end || {PlayerId, Pos, Points, Status} <- TableResultWithPos].


%% sort_results(Results) -> SortedResults
%% Types: Results = SortedResults = [{PlayerId, Points}]
%% Description: Sort the list from a best result to a lower one.
sort_results(Results) ->
    SF = fun({PId1, Points}, {PId2, Points}) -> PId1 =< PId2;
            ({_, Points1}, {_, Points2}) -> Points2 =< Points1
         end,
    lists:sort(SF, Results).

%% sort_results2(Results) -> SortedResults
%% Types: Results = SortedResults = [{PlayerId, Points, Status}] Status = active | eliminated
sort_results2(Results) ->
    SF = fun({PId1, Points, Status}, {PId2, Points, Status}) -> PId1 =< PId2;
            ({_PId1, Points1, Status}, {_PId2, Points2, Status}) -> Points2 =< Points1;
            ({_PId1, _Points1, _Status1}, {_PId2, _Points2, Status2}) -> Status2 == eliminated
         end,
    lists:sort(SF, Results).

%% replace_player_by_bot(PlayerId, TableId, SeatNum,
%%                       #state{players = Players, seats = Seats,
%%                              game_id = GameId, bots_params = BotsParams,
%%                              player_id_counter = NewPlayerId, tables = Tables,
%%                              tab_requests = Requests} = StateData) ->
%%     NewPlayers = del_player(PlayerId, Players),
%%     [#'PlayerInfo'{id = UserId} = UserInfo] = spawn_bots(GameId, BotsParams, 1),
%%     NewPlayers2 = reg_player(#player{id = NewPlayerId, user_id = UserId, is_bot = true}, NewPlayers),
%%     NewSeats = assign_seat(TableId, SeatNum, NewPlayerId, true, false, false, Seats),
%%     TablePid = get_table_pid(TableId, Tables),
%%     NewRequests = table_req_replace_player(TablePid, NewPlayerId, UserInfo, TableId, SeatNum, Requests),
%%     {next_state, ?STATE_PROCESSING, StateData#state{players = NewPlayers2,
%%                                                     seats = NewSeats,
%%                                                     player_id_counter = NewPlayerId + 1,
%%                                                     tab_requests = NewRequests}}.
%% 

%% table_req_replace_player(TablePid, PlayerId, UserInfo, TableId, SeatNum, TabRequests) ->
%%     RequestId = make_ref(),
%%     NewRequests = dict:store(RequestId, {replace_player, PlayerId, TableId, SeatNum}, TabRequests),
%%     send_to_table(TablePid, {replace_player, RequestId, UserInfo, PlayerId, SeatNum}),
%%     NewRequests.



%% prepare_players_for_new_tour(Tour, TTable, ToursPlan, Players) -> [{PlayerId, UserInfo, Points}]
prepare_players_for_new_tour(Tour, TTable, ToursPlan, Players) ->
    PrevTour = Tour - 1,
    TResult = ttable_get_tour_result(PrevTour, TTable),
    if Tour == 1 ->
           [{PlayerId, get_user_info(PlayerId, Players), _Points = 0}
            || {PlayerId, _, _, active} <- TResult];
       true ->
           case lists:nth(PrevTour, ToursPlan) of
               ne -> %% No one was eliminated => using the prev turn points
                   [{PlayerId, get_user_info(PlayerId, Players), Points}
                    || {PlayerId, _, Points, active} <- TResult];
               _ ->
                   [{PlayerId, get_user_info(PlayerId, Players), _Points = 0}
                    || {PlayerId, _, _, active} <- TResult]
           end
    end.


prepare_ttable_for_tables(TTable, Players) ->
    [{Tour, [{get_user_id(PlayerId, Players), Place, Score, Status}
             || {PlayerId, Place, Score, Status} <- Results]}
     || {Tour, Results} <- TTable].

%% setup_tables(TableModule, Players, PlayersPerTable, TTable, Tour, Tours, TableIdCounter, GameId, TableParams) ->
%%                              {Tables, Seats, NewTableIdCounter, CrRequests}
%% Types: Players = {PlayerId, UserInfo, Points}
%%        TTable = [{Tour, [{UserId, CommonPos, Score, Status}]}]
setup_tables(TableModule, Players, PlayersPerTable, TTable, Tour,
             Tours, TableIdCounter, GameId, TableParams) ->
    SPlayers = shuffle(Players),
    Groups = split_by_num(PlayersPerTable, SPlayers),
    F = fun(Group, {TAcc, SAcc, TableId, TCrRequestsAcc}) ->
                {TPlayers, _} = lists:mapfoldl(fun({PlayerId, UserInfo, Points}, SeatNum) ->
                                                       {{PlayerId, UserInfo, SeatNum, Points}, SeatNum+1}
                                               end, 1, Group),
                TableParams2 = [{players, TPlayers}, {ttable, TTable}, {tour, Tour},
                                {tours, Tours}, {parent, {?MODULE, self()}} | TableParams],
                {ok, TabPid} = spawn_table(TableModule, GameId, TableId, TableParams2),
                MonRef = erlang:monitor(process, TabPid),
                NewTAcc = reg_table(TableId, TabPid, MonRef, _GlTableId = 0, _Context = undefined, TAcc),
                F2 = fun({PlId, _, SNum, _}, Acc) ->
                             assign_seat(TableId, SNum, PlId, _Reg = false, _Conn = false, Acc)
                     end,
                NewSAcc = lists:foldl(F2, SAcc, TPlayers),
                PlayersIds = [PlayerId || {PlayerId, _, _} <- Group],
                NewTCrRequestsAcc = dict:store(TableId, PlayersIds, TCrRequestsAcc),
                {NewTAcc, NewSAcc, TableId + 1, NewTCrRequestsAcc}
        end,
    lists:foldl(F, {tables_init(), seats_init(), TableIdCounter, dict:new()}, Groups).


%% setup_players(Registrants) -> Players
setup_players(Registrants) ->
    F = fun(UserId, {Acc, PlayerId}) ->
                UserInfo = auth_server:get_user_info_by_user_id(UserId),
                NewAcc = store_player(#player{id = PlayerId, user_id = UserId,
                                              user_info = UserInfo, status = active}, Acc),
                {NewAcc, PlayerId + 1}
        end,
    {Players, _} = lists:foldl(F, {players_init(), 1}, Registrants),
    Players.


%% finalize_tables_with_rejoin(TableModule, Tables) -> ok
finalize_tables_with_rejoin(TableModule, Tables) ->
    F = fun(#table{mon_ref = MonRef, pid = TablePid}) ->
                erlang:demonitor(MonRef, [flush]),
                send_to_table(TableModule, TablePid, rejoin_players),
                send_to_table(TableModule, TablePid, stop)
        end,
    lists:foreach(F, tables_to_list(Tables)).

%% finalize_tables_with_rejoin(TableModule, Tables) -> ok
finalize_tables_with_disconnect(TableModule, Tables) ->
    F = fun(#table{mon_ref = MonRef, pid = TablePid}) ->
                erlang:demonitor(MonRef, [flush]),
                send_to_table(TableModule, TablePid, disconnect_players),
                send_to_table(TableModule, TablePid, stop)
        end,
    lists:foreach(F, tables_to_list(Tables)).


%% ttable_init(PlayersIds) -> TTable
%% Types: TTable = [{Tour, TourResult}]
ttable_init(PlayersIds) -> [{0, [{Id, Id, 0, active} || Id <- PlayersIds]}].

%% ttable_get_tour_result(Tour, TTable) -> undefined | TourResult
%% Types: TourResult = [{PlayerId, CommonPos, Points, PlayerState}]
%%          PlayerState = undefined | active | eliminated
ttable_get_tour_result(Tour, TTable) ->
    proplists:get_value(Tour, TTable).

%% ttable_store_tour_result(Tour, TourResult, TTable) -> NewTTable
ttable_store_tour_result(Tour, TourResult, TTable) ->
    lists:keystore(Tour, 1, TTable, {Tour, TourResult}).


%% players_init() -> players()
players_init() -> midict:new().

%% store_player(#player{}, Players) -> NewPlayers
store_player(#player{id =Id, user_id = UserId} = Player, Players) ->
    midict:store(Id, Player, [{user_id, UserId}], Players).

get_players_ids(Players) ->
    [P#player.id || P <- players_to_list(Players)].

get_player_by_user_id(UserId, Players) ->
    case midict:geti(UserId, user_id, Players) of
        [Player] -> {ok, Player};
        [] -> error
    end.

%% players_to_list(Players) -> List
players_to_list(Players) -> midict:all_values(Players).

get_user_info(PlayerId, Players) ->
    #player{user_info = UserInfo} = midict:fetch(PlayerId, Players),
    UserInfo.

get_user_id(PlayerId, Players) ->
    #player{user_id = UserId} = midict:fetch(PlayerId, Players),
    UserId.

set_player_status(PlayerId, Status, Players) ->
    Player = midict:fetch(PlayerId, Players),
    store_player(Player#player{status = Status}, Players).

tables_init() -> midict:new().

reg_table(TableId, Pid, MonRef, GlobalId, TableContext, Tables) ->
    Table = #table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId,
                   state = initializing, context = TableContext},
    store_table(Table, Tables).

update_created_table(TableId, Relay, Tables) ->
    Table = midict:fetch(TableId, Tables),
    NewTable = Table#table{relay = Relay, state = ?TABLE_STATE_READY},
    store_table(NewTable, Tables).

store_table(#table{id = TableId, pid = Pid, mon_ref = MonRef, global_id = GlobalId} = Table, Tables) ->
    midict:store(TableId, Table, [{pid, Pid}, {global_id, GlobalId}, {mon_ref, MonRef}], Tables).

fetch_table(TableId, Tables) -> midict:fetch(TableId, Tables).

get_table_pid(TabId, Tables) ->
    #table{pid = TabPid} = midict:fetch(TabId, Tables),
    TabPid.

get_table_by_mon_ref(MonRef, Tables) ->
    case midict:geti(MonRef, mon_ref, Tables) of
        [Table] -> Table;
        [] -> not_found
    end.

tables_to_list(Tables) -> midict:all_values(Tables).

seats_init() -> midict:new().

find_seats_by_player_id(PlayerId, Seats) ->
    midict:geti(PlayerId, player_id, Seats).

find_seats_by_table_id(TabId, Seats) ->
    midict:geti(TabId, table_id, Seats).

%% assign_seat(TabId, SeatNum, PlayerId, RegByTable, Connected, Seats) -> NewSeats
%% PlayerId = integer()
%% RegByTable = Connected = undefined | boolean()
assign_seat(TabId, SeatNum, PlayerId, RegByTable, Connected, Seats) ->
    Seat = #seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 registered_by_table = RegByTable, connected = Connected},
    store_seat(Seat, Seats).

update_seat_connect_status(TableId, SeatNum, ConnStatus, Seats) ->
    Seat = midict:fetch({TableId, SeatNum}, Seats),
    NewSeat = Seat#seat{connected = ConnStatus},
    store_seat(NewSeat, Seats).

store_seat(#seat{table = TabId, seat_num = SeatNum, player_id = PlayerId,
                 registered_by_table = _RegByTable,
                 connected = Connected} = Seat, Seats) ->
    Indices = if PlayerId == undefined ->
                     [{table_id, TabId}, {free, true}, {free_at_tab, TabId}];
                 true ->
                     [{table_id, TabId}, {free, false}, {non_free_at_tab, TabId},
                      {player_id, PlayerId}, {{connected, TabId}, Connected}]
              end,
    midict:store({TabId, SeatNum}, Seat, Indices, Seats).

user_id_to_string(UserId) -> binary_to_list(UserId).

shuffle(List) -> deck:to_list(deck:shuffle(deck:from_list(List))).

split_by_num(Num, List) -> split_by_num(Num, List, []).

split_by_num(_, [], Acc) -> lists:reverse(Acc);
split_by_num(Num, List, Acc) ->
    {Group, Rest} = lists:split(Num, List),
    split_by_num(Num, Rest, [Group | Acc]).

%% start_timer(Timeout) -> {TRef, Magic}
start_timer(Timeout) ->
    Magic = make_ref(),
    TRef = erlang:send_after(Timeout, self(), {timeout, Magic}),
    {TRef, Magic}.

spawn_table(TableModule, GameId, TableId, Params) -> TableModule:start(GameId, TableId, Params).

send_to_table(TableModule, TabPid, Message) -> TableModule:parent_message(TabPid, Message).


get_param(ParamId, Params) ->
    {_, Value} = lists:keyfind(ParamId, 1, Params),
    Value.

get_option(OptionId, Params, DefValue) ->
    proplists:get_value(OptionId, Params, DefValue).



