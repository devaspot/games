-module(tavla_test).
-author('Maxim Sokhatsky <maxim@synrc.com').

-include_lib("server/include/requests.hrl").
-include_lib("server/include/game_tavla.hrl").
-include_lib("server/include/settings.hrl").

-export([start/0]).

-define(BT, 30000).
-define(FLUSH_DELAY, 10).
-define(SIM_DELAY, 10).
-define(TCM, tc).

-record(state, {
        conn,
        gid,
        uid,
        mode,
        hand,
        pileh,
        rounds,
        current_round,
        acker_fun = fun(_Event) -> continue end }).

-record(bo, {pid, event, event_no, order }).

start() ->
    MultiOwner = self(),
    process_flag(trap_exit, true),
    Clients = [ proc_lib:spawn_link(fun() -> 
                         fire_starter(MultiOwner, join_game_humans, normal)
                 end) || _ <- lists:seq(1, 1) ],

    [ wait_for(C) || C <- Clients ].

fire_starter(MultiOwner, CreateMode, RevealMode) ->
    process_flag(trap_exit, true),
    Host = localhost,
    Port = 9000,
    Owner = self(),
    Rematch = 1,
    case CreateMode of

         join_game_humans ->

            Humans = [<<"maxim">>,<<"alice">>], % see authored users in auth_server.erl
            {ok, GameId, _A} = game:create_table(game_tavla, [{deny_robots,true},{rounds, 1}], Humans),
            gas:info(?MODULE,"created table for Tavla Game: gameid ~p",[{GameId,_A}]),
            Clients = [ proc_lib:spawn_link(fun() -> 
                                 timer:sleep(crypto:rand_uniform(0, 10)),
                                 attach_and_join(Owner, Host, Port, GameId, Id, Rematch, RevealMode)
                        end) || Id <- Humans ],

            gas:info(?MODULE,"Human Pids: ~p",[Clients]),
            Orders = []

    end,
    conductor(Orders, Clients),
    MultiOwner ! {self(), game_ended}.

attach_and_join(Owner, Host, Port, GameId, OwnId, Rematch, Mode) ->
    put(mode, Mode),
    log(started),
    S1 = ?TCM:connect(Host, Port),
    TT = case OwnId of
            <<"maxim">> -> ?TEST_TOKEN;
            <<"alice">> -> ?TEST_TOKEN2
    end,
    #'PlayerInfo'{id = Id} = ?TCM:call_rpc(S1, #session_attach{token = TT}) ,
    log(connected),
    ok = ?TCM:call_rpc(S1, #join_game{game = GameId}) ,
    State = #state{conn = S1, gid = GameId, uid = Id, acker_fun = standard_acker(Owner)},
    play_round(State, Rematch),
    log(finished),
    ok = ?TCM:flush_events(?FLUSH_DELAY),
    ok = ?TCM:close(S1).

% in tavla game we have only rounds (from 1 to 7) withing one game

play_round(State0, Rematch) ->
    _Id = State0#state.uid,
    State = init_tavla_roundroll(State0),
    loop_and_restart(State),
    play_round(State, Rematch).

loop_and_restart(State) ->
    LoopRes = tavla_client_loop(State#state{}),
    gas:info(?MODULE,"Human LoopRes: ~p",[LoopRes]),
    say_ready(State),
    case LoopRes of
        <<"done">> ->
            gas:info(?MODULE,"ID: ~p, next action done", [State#state.uid]),
            ok;
        <<"next_set">> ->
            gas:info(?MODULE,"ID: ~p, next action next_set", [State#state.uid]),
            ok;
        <<"next_round">> ->
            gas:info(?MODULE,"ID: ~p, next action next_round", [State#state.uid]),
            State1 = State#state{current_round = -1},
            check_ack(State1, game_ended, fun loop_and_restart/1, [State1])
    end.

init_tavla_roundroll(State) ->
    _GameId = State#state.gid,
    receive
        #'game_event'{event = <<"tavla_game_info">>, args = Args, game = _GI} ->
            GT = proplists:get_value(game_type, Args),
            Rounds = proplists:get_value(rounds, Args),
            CurrentRound = proplists:get_value(current_round, Args),
            log(game_info),
            State#state{mode = GT, current_round = CurrentRound, rounds = Rounds}
    after ?BT ->
            gas:info(?MODULE,"ERROR: ~p", [{server_timeout, "game_event:tavla_game_info"}]),
            erlang:error({server_timeout, "game_event:tavla_game_info"})
    end.

say_ready(State) ->
    S1 = State#state.conn,
    GameId = State#state.gid,
    <<"ok">> = ?TCM:call_rpc(S1, #game_action{game = GameId, action = tavla_ready, args = []}) .

tavla_client_loop(State) ->
    Id = State#state.uid,
    receive
        #'game_event'{event = <<"tavla_next_turn">>, args = Args} ->
            Timeout = crypto:rand_uniform(0, ?SIM_DELAY),
            State1 = case {proplists:get_value(player, Args), proplists:get_value(can_challenge, Args)} of
                         {Id, false} ->
                             do_turn(State, Timeout);
                         {_OtherId, _} ->
                             State
                     end,
            tavla_client_loop(State1);
        #'game_event'{event = <<"tavla_rolls">>} ->
            tavla_client_loop(State);
        #'game_event'{event = <<"tavla_moves">>} ->
            tavla_client_loop(State);
        #'game_event'{event = <<"tavla_ack">>} ->
            tavla_client_loop(State);
        #'game_event'{event = <<"tavla_game_ended">>, args = Args} ->
            %okey_round_ended_checks(Args, State),
            Reason = proplists:get_value(reason, Args),
            NextAction = proplists:get_value(next_action, Args),
            gas:info(?MODULE,"ID: ~p game ended, reason: ~p", [Id, Reason]),
            NextAction;
        #player_left{} ->
            tavla_client_loop(State);
        #'game_event'{event = <<"tavla_game_info">>, args = _Args} ->
            erlang:error({protocol_breach, okey_game_info});
        #'game_event'{event = <<"player_left">>} ->
            tavla_client_loop(State);
        _Msg ->
            gas:info(?MODULE,"the msg: ~p", [_Msg]),
            erlang:error({bot_received_unrecognized_message, _Msg})
    after ?BT ->
            log(server_timeouted),
            erlang:error({server_timeout, "tavla_client_loop_timeout"})
    end.

do_turn(State, _Timeout) ->
    RollAnswer = do_roll(State),
    log(RollAnswer),
    MoveAnswer = do_move(State),
    log(MoveAnswer),
    State.

do_roll(State) ->
    GameId = State#state.gid,
    S = State#state.conn,
    ZZZ = ?TCM:call_rpc(S, #game_action{
                         game = GameId,
                         action = tavla_roll,
                         args = []}),
    gas:info(?MODULE,"ID: ~p roll result: ~p", [State#state.uid, ZZZ]),
    ok.

do_move(State) ->
    GameId = State#state.gid,
    S = State#state.conn,
    Moves = [{from,2},{to, 25}],
    Player = State#state.uid,
    ZZZ = ?TCM:call_rpc(S, #game_action{
                         game = GameId,
                         action = tavla_move,
                         args = [ {moves, Moves}, {player, Player} ]}),
    gas:info(?MODULE,"ID: ~p move result: ~p", [State#state.uid, ZZZ]),
    ok.

log(Msg) ->
    ?TCM:log(Msg).

wait_for(_C) ->
    receive
        {_, game_ended} ->
            gas:info(?MODULE,"client: game ended");
        {'EXIT', _, normal} = M ->
            gas:info(?MODULE,"client: normal ending: ~p", [M]);
        {'EXIT', _C, Reason} = M ->
            gas:info(?MODULE,"client: FAILURE message: ~p", [{M,_C,Reason}]),
            erlang:error(Reason);
        M ->
            gas:info(?MODULE,"client: unrecognized result: ~p", [M]),
            erlang:error(M)
    end.

check_ack(State = #state{acker_fun = F}, Event, ContinueFun, Args) ->
    A = F(Event),
    case A of
        continue ->
            apply(ContinueFun, Args);
        {sleep, T} ->
            timer:sleep(T),
            apply(ContinueFun, Args);
        {do_and_continue, What, With} ->
            apply(What, [State] ++ With),
            apply(ContinueFun, Args);
        stop ->
            gas:info(?MODULE,"ID: ~p, Pid: ~p, check_ack. STOPPING THE BOT!!!", [State#state.uid, self()]),
            erlang:error({terminate, acker_stop})
    end.

standard_acker(Owner) ->
    Self = self(),
    Ref = make_ref(),
    fun(Event) ->
            E = {event, Ref, Self, Event},
            gas:info(?MODULE,"standard acker. ~p ! ~p", [Owner, E]),
            Owner ! E,
            receive
                {ARef, Res} when Ref == ARef ->
                    gas:info(?MODULE,"standard acker got '~p' answer on question ~p", [Res,{Owner, E}]),
                    Res
            end
    end.

update_events(Events, Pid, Event) ->
    F = [ X || {APid, AEvent, _} = X <- Events, Pid == APid, Event == AEvent ],
    case F of
        [] ->
            [{Pid, Event, 1} | Events];
        [{_,_,C} = X] ->
            L1 = lists:delete(X, Events),
            [{Pid, Event, C+1} | L1]
    end.

match_event(Orders, Events, Pid, Event) ->
    Z = [ X || {APid, AEvent, _} = X <- Events, Pid == APid, Event == AEvent ],
    [{_, _, C}] = Z,
    Matched = [ Y || Y = #bo{pid = P, event = E} <- Orders, P == Pid, E == Event ],
    case Matched of
        [BO = #bo{event_no = EN} | _] when EN == C ->
            BO;
        _X ->
            false
    end.

conductor(Orders, Clients) ->
    gas:info(?MODULE,"conductor init", []),
    Pairs = lists:zip(lists:seq(1, length(Clients)), Clients),
    Orders2 = lists:map(fun(O) ->
                                {_, P} = lists:keyfind(O#bo.pid, 1, Pairs),
                                O#bo{pid = P}
                        end, Orders),
    conductor(Orders2, Clients, []).
conductor(_Orders, [], _Events) ->
    gas:info(?MODULE,"conductor stop", []),
    ok;
conductor(Orders, Clients, Events) ->
    receive
        {event, Ref, Pid, Event} = _E ->
            E2 = update_events(Events, Pid, Event),
            Z = match_event(Orders, E2, Pid, Event),
            case Z of
                #bo{order = Order} ->
                    Pid ! {Ref, Order};
                _ ->
                    Pid ! {Ref, continue}
            end,
            conductor(Orders, Clients, E2);
        {C, game_ended} ->
            gas:info(?MODULE,"conductor: game ended", []),
            conductor(Orders, lists:delete(C, Clients), Events);
        {'EXIT', C, normal} = M ->
            gas:info(?MODULE,"conductor: normal ending: ~p", [M]),
            conductor(Orders, lists:delete(C, Clients), Events);
        {'EXIT', C, {terminate, acker_stop}} = M ->
            gas:info(?MODULE,"conductor: removing client ~p because of ~p", [C, M]),
            conductor(Orders, lists:delete(C, Clients), Events);
        {'EXIT', _C, Reason} = M ->
            gas:info(?MODULE,"conductor: FAILURE message: ~p", [{M,_C,Reason}]),
            erlang:error(Reason);
        M ->
            gas:info(?MODULE,"conductor: unrecognized msg from client: ~p", [M]),
            erlang:error(M)
    end.
