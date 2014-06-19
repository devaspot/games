-module(okey_bot).
-behaviour(gen_server).

-export([start/3, start/4, start_link/3, start_link/4, robot_init/1]).
-export([init_state/2, join_game/1, get_session/1]).
-export([send_message/2]).
-export([call_rpc/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("server/include/requests.hrl").
-include_lib("server/include/game_okey.hrl").

-define(GAME_SESSION, game_session).

-record(state, {
        is_robot = true :: boolean(),
        conn :: pid(),
        mode :: atom(),
        hand = [] :: list(#'OkeyPiece'{}),
        gosterge :: #'OkeyPiece'{},
        set_state :: #'OkeySetState'{},
        delay :: integer(),
        user :: #'PlayerInfo'{},
        uid :: 'PlayerId'(),
        owner :: pid(),
        owner_mon,
        session :: pid(),
        gid :: 'GameId'(),
        bot :: pid(),
        okey_disable = false :: boolean(),
        running_requests = dict:new() :: any(),
        request_id = 0,
        game_session = ?GAME_SESSION
    }).

send_message(Pid, Message) -> gen_server:call(Pid, {server, Message}).
call_rpc(Pid, Message) -> gen_server:call(Pid, {call_rpc, Message}).
get_session(Pid) -> gen_server:call(Pid, get_session).
init_state(Pid, Situation) -> gen_server:cast(Pid, {init_state, Situation}).
join_game(Pid) -> gen_server:cast(Pid, join_game).
start(Owner, PlayerInfo, GameId) -> gen_server:start(?MODULE, [Owner, PlayerInfo, GameId], []).
start(Owner, PlayerInfo, GameId, GameSession) -> gen_server:start(?MODULE, [Owner, PlayerInfo, GameId, GameSession], []).
start_link(Owner, PlayerInfo, GameId) -> gen_server:start_link(?MODULE, [Owner, PlayerInfo, GameId], []).
start_link(Owner, PlayerInfo, GameId, GameSession) -> gen_server:start_link(?MODULE, [Owner, PlayerInfo, GameId, GameSession], []).

init([Owner, PlayerInfo, GameId]) ->
    {ok, SPid} = ?GAME_SESSION:start_link(self()),
    ?GAME_SESSION:bot_session_attach(SPid, PlayerInfo),
    UId = PlayerInfo#'PlayerInfo'.id,
    gas:info(?MODULE,"BOTMODULE ~p started with game_session pid ~p", [UId,SPid]),
    {ok, #state{user = PlayerInfo, uid = UId, owner = Owner, gid = GameId, session = SPid, game_session = ?GAME_SESSION}};
init([Owner, PlayerInfo, GameId, GameSession]) ->
    {ok, SPid} = GameSession:start_link(self()),
    GameSession:bot_session_attach(SPid, PlayerInfo),
    UId = PlayerInfo#'PlayerInfo'.id,
    gas:info(?MODULE,"BOTMODULE ~p started with game_session pid ~p game session ~p", [UId, SPid, GameSession]),
    {ok, #state{user = PlayerInfo, uid = UId, owner = Owner, gid = GameId, session = SPid}}.

handle_call({server, Msg0}, _From, #state{uid = UId, bot = BPid} = State) ->
    Msg = flashify(Msg0),
    gas:info(?MODULE,"OKEY BOT ~p: Resend message to bot process (~p): ~p",[UId, BPid, Msg0]),
    BPid ! Msg,
    {reply, ok, State};

handle_call({call_rpc, Msg}, From, #state{game_session = GameSession} = State) ->
    gas:info(?MODULE,"OKEY BOT: call_rpc ~p",[Msg]),
    RR = State#state.running_requests,
    Id = State#state.request_id + 1,
    Self = self(),
    RR1 = dict:store(Id, From, RR),
    proc_lib:spawn_link(fun() ->
                                Res = try
                                          Answer = GameSession:process_request(State#state.session, "OKEY BOT", Msg),
                                          gas:info(?MODULE,"Process Request from OKEY BOT:",[]),
                                          gas:info(?MODULE,"                      REQUEST: ~p",[Msg]),
                                          gas:info(?MODULE,"                        REPLY: ~p",[Answer]),
                                          {reply, Id, Answer}
                                      catch
                                          _Err:Reason ->
                                              {reply, Id, {error, Reason}}
                                      end,
                                gen_server:call(Self, Res)
                        end),
    {noreply, State#state{running_requests = RR1, request_id = Id, okey_disable = false}};

handle_call({reply, Id, Answer}, _From, State) ->
    RR = State#state.running_requests,
    From = dict:fetch(Id, RR),
    gen_server:reply(From, Answer),
    {reply, ok, State};

handle_call(get_session, _From, State) ->
    {reply, State#state.session, State};

handle_call(Request, _From, State) ->
    Reply = ok,
    gas:info(?MODULE,"unknown call: ~p", [Request]),
    {reply, Reply, State}.

handle_cast(join_game, State) ->
    Mon = erlang:monitor(process, State#state.owner),
    UId = State#state.uid,
    GId = State#state.gid,
    BPid = proc_lib:spawn_link(okey_bot, robot_init, [#state{gid = GId, uid = UId, conn = self()}]),
    BPid ! join_game,
    {noreply, State#state{bot = BPid, owner_mon = Mon}};

handle_cast(Msg, State) ->
    gas:info(?MODULE,"unknown cast: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, Reason},
            State = #state{owner_mon = OMon}) when OMon == Ref->
    gas:info
    ("relay goes down with reason ~p so does bot", [Reason]),
    {stop, Reason, State};
handle_info({server,M}, #state{uid = UId, bot = BPid} = State) ->
    BPid ! M,
    {noreply, State};
handle_info(Info, State) ->
    {stop, {unrecognized_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

flashify(R) when is_tuple(R) ->
    [RecName | Rest] = tuple_to_list(R),
    Rest1 = lists:map(fun
                          (X) -> flashify(X)
                      end, Rest),
    list_to_tuple([RecName | Rest1]);
flashify([{Key, _Value} | _] = P) when is_atom(Key) ->
    lists:map(fun
                  ({K, V}) when is_atom(K) -> {K, flashify(V)}
              end, P);
flashify(A) when A == true -> A;
flashify(A) when A == false -> A;
flashify(A) when A == null -> A;
flashify(A) when A == undefined -> A;
flashify(A) when is_atom(A) ->
    list_to_binary(atom_to_list(A));
flashify(Other) ->
    Other.

%%------------------------ BOT LOGIC

robot_init(State) ->
    robot_init_loop(State).

robot_init_loop(State) ->
    S = State#state.conn,
    Id = State#state.uid,
    GameId = State#state.gid,
    receive
        join_game ->
            gas:info(?MODULE,"OKEY BOT JOINED",[]),
            case call_rpc(S, #join_game{game = GameId}) of
                {error, _Err} ->
                    gas:info(?MODULE,"ID: ~p failed take with msg ~p", [Id, _Err]),
                    erlang:error(robot_cant_join_game);
                _ ->
                    okey_client_loop(State)
            end;
        _X ->
           gas:info(?MODULE,"OKEY BOT X: ~p",[_X])
    end.

okey_client_loop(State) ->
    gas:info(?MODULE,"OKEY BOT CLIENT LOOP", []),
    Hand0 = State#state.hand,
    Id = State#state.uid,
    receive
        #game_event{event = okey_next_turn, args = Args} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            Hand1 = case {proplists:get_value(player, Args), proplists:get_value(can_challenge, Args)} of
                        {Id, false} ->
                            gas:info(?MODULE,"OKEY NEXT TURN",[]),
                            do_turn(State, Hand0);
                        {_OtherId, _Val} ->
                            Hand0
                    end,
            okey_client_loop(State#state{hand = Hand1});
        #game_event{event = okey_revealed} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            do_challenge(State),
            okey_client_loop(State);
        #game_event{event = okey_series_ended} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
%%            S = State#state.conn,
%%            call_rpc(S, #logout{});
            okey_client_loop(State);
        #game_event{event = okey_round_ended, args = Args} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            NextAction = proplists:get_value(next_action, Args),
            gas:info(?MODULE,"ID: ~p round ended", [Id]),
            okey_client_round(NextAction, State);
        #game_event{event = okey_game_info, args = Args} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            Mode = proplists:get_value(game_type, Args),
            SM = proplists:get_value(sets, Args),
            SC = proplists:get_value(set_no, Args),
            RM = proplists:get_value(rounds, Args),
            TO = proplists:get_value(timeouts, Args),
%%            Speed = TO#'OkeyTimeouts'.speed,
%%            SpeedAtom = list_to_atom(binary_to_list(Speed)),
            Delay = get_delay(fast),
            ST = #'OkeySetState'{round_cur = 1, round_max = RM, set_cur = SC, set_max = SM},
            okey_client_loop(State#state{set_state = ST, delay = Delay, mode = Mode});
        #game_event{event = okey_enable, args = Args} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            okey_client_loop(State#state{okey_disable = true});
        #game_event{event = okey_game_started, args = Args} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            MH = proplists:get_value(tiles, Args),
            G = proplists:get_value(gosterge, Args),
            RC = proplists:get_value(current_round, Args),
            ST = State#state.set_state,
            ST1 = ST#'OkeySetState'{round_cur = RC},
%            State#state{hand = MH, gosterge = G, set_state = ST1},
            gas:info(?MODULE,"OKEY BOT GAME STARTED : ~p",[length(MH)]),
            okey_client_loop(State#state{hand = MH, gosterge = G, set_state = ST1});
        #game_event{event = okey_game_player_state, args = Args} = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received message: ~p", [Id, Msg]),
            SS = #'OkeySetState'{round_cur = 1, round_max = 3,
                            set_cur = 1,set_max = 1},

            Pause = proplists:get_value(paused, Args, false),
            Turn = proplists:get_value(whos_move, Args),
            GameState = proplists:get_value(game_state, Args),
            Gosterge = State#state.gosterge,
            gas:info(?MODULE,"Id ~p Turn: ~p GameState: ~p",[Id, Turn,GameState]),

            case Pause of
                 true -> wait_for_resume();
                 false -> ok
            end,


            case {Turn, GameState} of
                {Id, do_okey_take} ->
                    gas:info(?MODULE,"init bot: take", []),
                    Hand1 = do_turn(State, Hand0),
                    okey_client_loop(State#state{hand = Hand1, gosterge = Gosterge});
                {Id, do_okey_discard} ->
                    gas:info(?MODULE,"init bot: discard", []),
                    {TryDiscard, _} = draw_random(Hand0),
                    Hand1 = do_discard(State, Hand0, TryDiscard),
                    okey_client_loop(State#state{hand = Hand1, gosterge = Gosterge});
                {_, game_finished} ->
                    gas:info(?MODULE,"init bot: finished", []),
                    okey_client_loop(State);
                {_, do_okey_ready} ->
                    gas:info(?MODULE,"init bot: ready", []),
                    say_ready(State),
                    okey_client_round(<<"done">>, State);
                {_, do_okey_challenge} ->
                    gas:info(?MODULE,"init bot: challenge", []),
                    do_challenge(State),
                    okey_client_loop(State#state{hand = Hand0, gosterge = Gosterge});
                {_, _B} ->
                    gas:info(?MODULE,"init bot: UNKNOWN ~p", [_B]),
                    okey_client_loop(State#state{hand = Hand0, gosterge = Gosterge})
            end;

        _Other = Msg ->
            gas:info(?MODULE,"OKEY_BOT <~p> : Received unhandled message: ~p", [Id, Msg]),
            okey_client_loop(State)
    end.

do_challenge(State) ->
    GameId = State#state.gid,
    S = State#state.conn,
    ZZZ = call_rpc(S, #game_action{
                         game = GameId,
                         action = okey_challenge,
                         args = [ {challenge, random_bool(0.2)
                                             } ]}),
    gas:info(?MODULE,"ID: ~p challenge result: ~p", [State#state.uid, ZZZ]),
    ok.

okey_client_round(next_round, State) ->
    %% round ended, wait for new round info
%    State2 = get_hand(State),
    okey_client_loop(State);
okey_client_round(UnknowAction, State) ->
    gas:info(?MODULE,"OKEY BOT unknow action ~p", [UnknowAction]),
    okey_client_loop(State).


okey_client_rematch2(State) ->
    S = State#state.conn,
    GameId = State#state.gid,
    gas:info(?MODULE,"rematch loop receive", []),
    receive
        #game_event{event = <<"player_left">>, args = Args} ->
            gas:info(?MODULE,"#game_event{event = <<\"player_left\">>, args = Args}", []),
            Replaced = proplists:get_value(bot_replaced, Args, false) orelse
                proplists:get_value(human_replaced, Args, false),
            case Replaced of
                false ->
                    call_rpc(S, #logout{});
                true ->
                    okey_client_rematch2(State)
            end
    end.

say_ready(State) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, action = okey_ready, args = []}).

do_turn(#state{delay = Delay} = State, Hand) ->
    Hand1 = if length(Hand) == 15 ->
                   Hand;
               true ->
                   gas:info(?MODULE,"OKEY BOT TAKE ? ~p ",[length(Hand)]),
                   simulate_delay(take, Delay),
                   {_, H1} = do_take(State, Hand),
                   H1
            end,
    true = is_list(Hand1),
    {TryDiscard, _} = draw_random(Hand1),
    gas:info(?MODULE,"DO TURN",[]),
    simulate_delay(discard, Delay),
    do_discard(State, Hand1, TryDiscard).

time_to_sleep(take, Delay) ->
    erlang:trunc((Delay / 3) * 1);
time_to_sleep(discard, Delay) ->
    erlang:trunc((Delay / 3) * 2).

simulate_delay(Action, Delay) ->
    TheDelay = time_to_sleep(Action, Delay),
    receive
        #game_paused{action = pause} ->
            wait_for_resume()
    after TheDelay ->
            ok
    end.

wait_for_resume() ->
    receive
        #game_paused{action = resume} ->
            ok
    end.

do_take(State, Hand) ->
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    Pile = case State#state.okey_disable of
                true -> 0;
                false -> crypto:rand_uniform(0, 2)
           end,
    case call_rpc(S, #game_action{
                        game = GameId,
                        action = okey_take,
                        args = [ {pile, Pile} ]}) of
        #'OkeyPiece'{} = Tosh ->
            MyHand = [Tosh | Hand],
            {false, MyHand};
        {error, cant_take_do_discard} ->
            {false, Hand};
        {error, game_has_already_ended} when State#state.mode == <<"countdown">> ->
            {false, Hand};
        _Err ->
            gas:info(?MODULE,"ID: ~p failed take with msg ~p", [Id, _Err]),
            erlang:error(failed_take),
            {false, Hand}
    end.

do_discard(State, Hand, Item) ->
    S = State#state.conn,
    GameId = State#state.gid,
    Hand1 = lists:delete(Item, Hand),
    _Res = call_rpc(S, #game_action{game = GameId, action = okey_discard,
                                        args = [ {tile, Item} ]}),
    Hand1.

draw_random([One]) ->
    {One, []};
draw_random(List) ->
    {is_list, true} = {is_list, is_list(List)},
    Pos = crypto:rand_uniform(1, length(List)),
    Item = lists:nth(Pos, List),
    ResList = lists:delete(Item, List),
    {Item, ResList}.

random_bool(Prob) ->
%    Point = crypto:rand_uniform(0, 1000),
%    Prob*1000 > Point,
    crypto:rand_uniform(0,2) == 0.

get_delay(fast) -> {ok, Val}   = kvs:get(config,"games/okey/robot_delay_fast", 1000), Val;
get_delay(normal) -> {ok, Val} = kvs:get(config,"games/okey/robot_delay_normal", 9000), Val;
get_delay(slow) -> {ok, Val}   = kvs:get(config,"games/okey/robot_delay_slow", 15000), Val.

