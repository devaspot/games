-module(game_session).
-behaviour(gen_server).
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, { user = undefined, rpc, rpc_mon, games = [] }).
-record(participation, {
        game_id       :: 'GameId'(),
        reg_num       :: integer(),
        rel_module    :: atom(),
        rel_pid       :: pid(), %% relay, which handles communication gameman maps game_id onto pid
        tab_module    :: atom(),
        tab_pid       :: pid(),
        ref           :: any(), %% monitor reference to relay
        role = viewer :: atom() %% [viewer, player, ghost]
       }).

start(RPC) when is_pid(RPC) -> gen_server:start(?MODULE, [RPC], []).
start_link(RPC) when is_pid(RPC) -> gen_server:start_link(?MODULE, [RPC], []).
bot_session_attach(Pid, UserInfo) -> gen_server:cast(Pid, {bot_session_attach, UserInfo}).
process_request(Pid, Msg) -> gen_server:call(Pid, {client_request, Msg}).
process_request(Pid, Source, Msg) -> gen_server:call(Pid, {client_request, Msg}).
send_message_to_player(Pid, Message) -> Pid ! {server,Message}, ok.

init([RPC]) -> MonRef = erlang:monitor(process, RPC), 
   {ok, #state{rpc = RPC, rpc_mon = MonRef}}.
handle_call({client_request, Request}, From, State) -> handle_client_request(Request, From, State);
handle_call(Request, From, State) ->
    gas:info(?MODULE,"Unrecognized call: ~p from ~p", [Request,From]),
    {stop, {unknown_call, From, Request}, State}.

handle_cast({bot_session_attach, UserInfo}, State = #state{user = undefined}) ->
    {noreply, State#state{user = UserInfo}};

handle_cast(Msg, State) ->
    gas:info(?MODULE,"Unrecognized cast: ~p", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

handle_info({relay_event, SubscrId, RelayMsg}, State) ->
    handle_relay_message(RelayMsg, SubscrId, State);

handle_info({relay_kick, SubscrId, Reason}, State) ->
    gas:info(?MODULE,"Recived a kick notification from the table: ~p", [Reason]),
    handle_relay_kick(Reason, SubscrId, State);

handle_info({'DOWN', MonitorRef, _Type, _Object, _Info} = Msg, State = #state{rpc_mon = MonitorRef}) ->
    gas:info(?MODULE, "connection closed, shutting down session:~p", [Msg]),
    {stop, normal, State};

handle_info({'DOWN', OtherRef, process, _Object, Info} = _Msg, #state{games = Games, rpc = RPC} = State) ->
    case lists:keyfind(OtherRef, #participation.ref, Games) of
        #participation{} ->
            gas:info(?MODULE,"The table is down: ~p", [Info]),
            gas:info(?MODULE,"Closing the client and sutting down the session.", []),
            send_message_to_player(RPC,
                #disconnect{reason_id = <<"tableDown">>,
                    reason = <<"The table you are playing on is unexpectedly down.">>}),
            {stop, table_down, State};
        _ -> {noreply, State} end;

handle_info(Info, State=#state{rpc=RPC}) ->
    gas:info(?MODULE,"Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #state{rpc=RPC}) ->
    gas:info(?MODULE,"Terminating session: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Register User

register_user_by_module(RPC,User,GameId,#state{games=Games} = State) ->
    UserId = User#'PlayerInfo'.id, 
    case game:get_relay_mod_pid(GameId) of
        {FLMod, FLPid} ->
            gas:info(?MODULE,"Found the game: ~p by module ~p Trying to register...",[FLPid,FLMod]),
            case FLMod:reg(FLPid, User) of
                {ok, {RegNum, {RMod, RPid}, {TMod, TPid}}} ->
                    gas:info(?MODULE,"join to game relay: ~p",[{RMod, RPid}]),
                    {ok, _SubscrId} = RMod:subscribe(RPid, self(), UserId, RegNum),
                    Ref = erlang:monitor(process, RPid),
                    Part = #participation{
                        ref = Ref, game_id = GameId, reg_num = RegNum,
                        rel_module = RMod, rel_pid = RPid,
                        tab_module = TMod, tab_pid = TPid, role = player},
                    NewGames = game:plist_setkey(GameId,#participation.game_id,Games,Part),
                    {reply, ok, State#state{games = NewGames}};
                {error, finished} ->
                    gas:info(?MODULE,"The game is finished: ~p.",[GameId]),
                    ok = send_message_to_player(RPC, #disconnect{reason_id = <<"gameFinished">>, reason = null}),
                    {reply, {error, finished}, State};
                {error, out} ->
                    gas:info(?MODULE,"Out of the game: ~p.",[GameId]),
                    ok = send_message_to_player(RPC, #disconnect{reason_id = <<"disconnected">>,reason = null}),
                    {reply, {error, out}, State};
                {error, not_allowed} ->
                    gas:error(?MODULE,"Not allowed to connect: ~p.",[GameId]),
                    ok = send_message_to_player(RPC, #disconnect{reason_id = <<"notAllowed">>,reason = null}),
                    {reply, {error, not_allowed}, State};
                UnknownError ->
                    gas:error(?MODULE,"Unknown Registration Error: ~p.",[GameId]),
                    ok = send_message_to_player(RPC, #disconnect{reason_id = term_to_binary(UnknownError),reason = null}),
                    {reply, UnknownError, State}
            end;
        undefined ->
            gas:error(?MODULE,"Game not found: ~p.",[GameId]),
            ok = send_message_to_player(RPC, #disconnect{reason_id = <<"notExists">>, reason = null}),
            {reply, {error, not_exists}, State}
    end.


%%===================================================================

handle_client_request(#session_attach{token = Token}, _From, #state{user = undefined} = State) ->
    gas:info(?MODULE,"Checking session token: ~p", [Token]),
    case auth_server:get_user_info(wf:to_binary(Token)) of
        false ->
            gas:error(?MODULE,"failed session attach: ~p", [Token]),
            {stop, normal, {error, invalid_token}, State};
        UserInfo ->
            gas:info(?MODULE,"successfull session attach. Your user info: ~p", [UserInfo]),
            {reply, UserInfo, State#state{user = UserInfo}} end;

handle_client_request(_, _From, #state{user = undefined} = State) ->
    gas:info(?MODULE,"Unknown session call", []),
    {reply, {error, do_session_attach_first}, State};

handle_client_request(#logout{}, _From, State) ->
    gas:info(?MODULE,"Logout", []),
    {stop, normal, ok, State};

handle_client_request(#stats_action{player_id = PlayerId, game_type = GameModule}, _From, #state{rpc = RPC} = State) ->
    Res = game:get_player_info(GameModule, PlayerId),
    gas:info(?MODULE,"Get player stats: ~p", [Res]),
    send_message_to_player(RPC, Res),
    {reply, Res, State};

handle_client_request(#chat{game = GameId, who = DisplayName, message = Msg0}, _From,
                      #state{user = User, games = Games} = State) ->
    gas:info(?MODULE,"Chat Message ~n ~p", [Msg0]),
    Msg = #chat_event{game = GameId, message = Msg0, player_id = User#'PlayerInfo'.id, who = DisplayName },
    Participation = get_relay(GameId, Games),
    Res = case Participation of
              false ->
                  {error, chat_not_registered};
              #participation{rel_pid = Srv, rel_module = RMod} ->
                  RMod:publish(Srv, Msg)
          end,
    {reply, Res, State};

handle_client_request(#join_game{game = GameId}, _From,
                      #state{user = User, rpc = RPC, games = Games} = State) ->
    UserId = User#'PlayerInfo'.id,
    gas:info(?MODULE,"Join game ~p user ~p from ~p", [GameId, UserId,_From]),
    case get_relay(GameId, Games) of
        #participation{} -> {reply, {error, already_joined}, State};
        false -> register_user_by_module(RPC,User,GameId,State) end;

handle_client_request(#game_action{game = GameId} = Msg, _From, State) ->
    gas:info(?MODULE,"Game action ~p", [{GameId,Msg,_From}]),
    Participation = get_relay(GameId, State#state.games),
    case Participation of
        false ->
            {reply, {error, game_not_found}, State};
        #participation{reg_num = RegNum, tab_pid = TPid, tab_module = TMod} ->
            UId = (State#state.user)#'PlayerInfo'.id,
            gas:info(?MODULE,"PLAYER ~p MOVES ~p in GAME ~p",[UId,Msg,GameId]),
            {reply, TMod:submit(TPid, RegNum, Msg), State}
    end;


handle_client_request(#pause_game{game = GameId, action = Action}, _From, State) ->

    Participation = get_relay(GameId, State#state.games),
    gas:info(?MODULE,"Pause game: ~p, user: ~p games: ~p",
        [GameId, State#state.user, State#state.games]),

    case Participation of
        false ->
            gas:info(?MODULE,"A", []),
            {reply, {error, game_not_found}, State};
        #participation{reg_num = RegNum, tab_pid = TPid, tab_module = TMod} ->
            Signal = case Action of
                         pause -> pause_game;
                         resume -> resume_game
                     end,
            Res = TMod:signal(TPid, RegNum, {Signal, self()}),
            gas:info(?MODULE,"B. Res: ~p", [Res]),
            {reply, Res, State}
    end;

handle_client_request(Request, _From, State=#state{rpc=RPC}) ->
    gas:info(?MODULE,"unrecognized client request: ~p", [Request]),
    {stop, {unknown_client_request, Request}, State}.

%%===================================================================

handle_relay_message(Msg, _SubscrId, #state{rpc = RPC} = State) ->
    try send_message_to_player(RPC, Msg) of
        ok -> {noreply, State};
        tcp_closed -> {stop, normal, State};
        E -> {stop, normal, State}
    catch exit:{normal, {gen_server,call, [RPC, {server, _}]}} -> {stop, normal, State};
          exit:{noproc, {gen_server,call, [RPC, {server, _}]}} -> {stop, normal, State};
          E:R -> {stop, normal, State} end.

%%===================================================================

%% The notification from the current table to rejoin to the game
%% because the user for example was moved to another table.
handle_relay_kick({rejoin, GameId}, _SubscrId,
                  #state{user = User, games = Games, rpc = RPC} = State) ->
    gas:info(?MODULE,"Rejoin request from relay. GameId ~p User ~p",[GameId,User#'PlayerInfo'.id]),
    register_user_by_module(RPC,User,GameId,State);

handle_relay_kick(Reason, _SubscrId, #state{rpc = RPC} = State) ->
    {ReasonId, ReasonText} =
        case Reason of
            table_closed -> {<<"tableClosed">>, null};
            table_down -> {null, <<"The table was closed unexpectedly.">>};
            game_over -> {null, <<"The game is over.">>};
            _ -> {<<"kicked">>, null}
        end,
    send_message_to_player(RPC, #disconnect{reason_id = ReasonId, reason = ReasonText}),
    {stop, normal, State}.

%%===================================================================

get_relay(GameId, GameList) ->
    lists:keyfind(GameId, #participation.game_id, GameList).
