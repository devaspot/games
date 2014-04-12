-module(test_okey).

-include_lib("eunit/include/eunit.hrl").
-include_lib("server/include/log.hrl").
-include_lib("server/include/settings.hrl").
-include_lib("server/include/conf.hrl").
-include_lib("server/include/kamf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/classes.hrl").
-include_lib("server/include/game_okey.hrl").

%% debug
-export([validate_hand/3,conductor/3]).
-export([start/0, alt_play_3_players_1_bot/0, init_with_join_game/7]).

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
          gosterge,
          pileh,
          acker_fun = fun(_Event) -> continue end,
          set_state :: #'OkeySetState'{}
         }).

%% bot order
-record(bo, {
          pid,
          event,
          event_no,
          order
         }).

%% ===================================================================
%% ===================================================================
%% Tests
%% ===================================================================
%% ===================================================================

game_creation_test_() ->
    {foreach,
     fun() -> tests:setup() end,
     fun(State) -> tests:cleanup(State) end,
     [
      % {timeout, 100, fun test_join_game_random_reveal/0},
      % {timeout, 100, fun test_match_me_random_reveal/0},
      % {timeout, 100, fun create_game_with_robots/0},
      % {timeout, 100, fun test_join_game_observer_settings/0},
      {timeout, 100, fun test_social_actions/0},
      fun() -> ok end
     ]
    }.

game_ending_test_() ->
    {foreach,
     fun() -> tests:setup() end,
     fun(State) -> tests:cleanup(State) end,
     [
      % {timeout, 100, fun test_join_game_empty_pile/0},
      % {timeout, 100, fun test_join_game_countdown_random_reveal/0},
      fun() -> ok end
     ]
    }.

replacement_test_() ->
    {foreach,
     fun() -> tests:setup() end,
     fun(State) -> tests:cleanup(State) end,
     [
      % {timeout, 100, fun alt_play_3_players_1_bot/0},
      % {timeout, 100, fun player_replacement/0},
      fun() -> ok end
     ]
    }.

matchmaker_test_() ->
    {foreach,
     fun() -> tests:setup() end,
     fun(State) -> tests:cleanup(State) end,
     [
      % {timeout, 100, fun matchmaker_disconnect/0},
      fun() -> ok end
     ]
    }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    tests setup                         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%player_replacement() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),
%    Clients = [ proc_lib:spawn_link(fun() ->
%                          start_test_game_t(MultiOwner, join_game_player_replacement, empty_pile)
%                 end) || _ <- lists:seq(1, 1) ],
%
%    [ wait_for(C) || C <- Clients ].

start() ->
    MultiOwner = self(),
    process_flag(trap_exit, true),
    Clients = [ proc_lib:spawn_link(fun() -> 
                         start_test_game_t(MultiOwner, join_game_robots, normal)
                 end) || _ <- lists:seq(1, 1) ],

    [ wait_for(C) || C <- Clients ].

alt_play_3_players_1_bot() ->
    MultiOwner = self(),
    process_flag(trap_exit, true),
    Clients = [ proc_lib:spawn_link(fun() ->
                         start_test_game_t(MultiOwner, join_game_new, normal)
                 end) || _ <- lists:seq(1, 1) ],

    [ wait_for(C) || C <- Clients ].

%matchmaker_disconnect() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),
%    Clients = [ proc_lib:spawn_link(fun() -> 
%                         start_test_game_t(MultiOwner, match_me_disconnect, normal) 
%                 end) || _ <- lists:seq(1, 1) ],%
%
%    [ wait_for(C) || C <- Clients ].

%test_join_game_random_reveal() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),
%    Clients = [ proc_lib:spawn_link(fun() -> 
%                         start_test_game_t(MultiOwner, join_game, normal) end) || _ <- lists:seq(1, 1) ],
%    [ wait_for(C) || C <- Clients ].

%test_join_game_countdown_random_reveal() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),%
%
%    Clients = [ proc_lib:spawn_link(fun() -> %
%			 start_test_game_t(MultiOwner, join_game_countdown, normal)
%                end) || _ <- lists:seq(1, 1) ],
%
 %   [ wait_for(C) || C <- Clients ].

%test_join_game_observer_settings() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),
%    Clients = [ proc_lib:spawn_link(fun() ->
%                         start_test_game_t(MultiOwner, join_game_observer, normal) 
%                end) || _ <- lists:seq(1, 1) ],
%
%    [ wait_for(C) || C <- Clients ].

%test_join_game_empty_pile() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),
%    Clients = [ proc_lib:spawn_link(fun() -> 
%			 start_test_game_t(MultiOwner, join_game, empty_pile) 
%		end) || _ <- lists:seq(1, 1) ],
%
%   [ wait_for(C) || C <- Clients ].

%test_match_me_random_reveal() ->
%    MultiOwner = self(),
%    process_flag(trap_exit, true),
%    Clients = [ proc_lib:spawn_link(fun() -> 
%			 start_test_game_t(MultiOwner, match_me, normal)
%		end) || _ <- lists:seq(1, 1) ],
%
 %   [ wait_for(C) || C <- Clients ].

test_social_actions() ->
    MultiOwner = self(),
    process_flag(trap_exit, true),
    Clients = [ proc_lib:spawn_link(fun() -> 
			 start_test_game_t(MultiOwner, test_social_actions, normal) 
                end) || _ <- lists:seq(1, 1) ],

    [ wait_for(C) || C <- Clients ].

start_test_game_t(MultiOwner, CreateMode, RevealMode) ->
    process_flag(trap_exit, true),
    Ids = [<<"radistao">>,<<"paul">>,<<"kunthar">>,<<"gleber">>],
    Host = localhost,
    Port = ?LISTEN_PORT,
    Owner = self(),
    Rematch = 0,
    case CreateMode of

        join_game_player_replacement ->

            ReplacementId = <<"kate">>,

            {ok, GameId, _} = game_manager:create_table(game_okey, 
					 [{allow_replacement, true}, {deny_robots, true},
					  {sets, 1}, {rounds, 2}], Ids),

            Clients0 = [ proc_lib:spawn_link(fun() -> 
				  init_with_join_game(Owner, Host, Port, GameId, Id, 0, RevealMode) 
		       end) || Id <- Ids ],

            Replacement = proc_lib:spawn_link(fun() ->
				   init_with_match_me_replaceable(Owner, Host, Port, ReplacementId, 0, RevealMode) 
			  end),

            gas:info(?MODULE,"replacement players pid: ~p", [Replacement]),

            Others = [],
            Clients = Clients0 ++ [Replacement] ++ Others,
            Orders = [#bo{pid = 2, event = game_ended, event_no = 1, order = stop}];

        join_game_robots ->

            Robots = [robot,robot,robot],
            Humans = [<<"paul">>],%<<"radistao">>, <<"paul">>],
            {ok, GameId, _A} = game_manager:create_table(game_okey, [{sets,2}, {rounds,20},{game_mode,color}], Robots ++ Humans),

	    gas:info(?MODULE,"created table for Okey Game: gameid ~p",[{GameId,_A}]),

            Clients = [ proc_lib:spawn_link(fun() -> 
				 timer:sleep(crypto:rand_uniform(0, 10)),
                                 init_with_join_game(Owner, Host, Port, GameId, Id, Rematch, RevealMode)
                        end) || Id <- Humans ],

            gas:info(?MODULE,"Human Pids: ~p",[Clients]),
            
            Orders = [];

        join_game ->

            {ok, GameId, _} = game_manager:create_table(game_okey, [{game_mode, color}, {sets, 2}, {rounds, 2}], Ids),

            Orders = [],

            Clients = [ proc_lib:spawn_link(fun() -> 
				 timer:sleep(crypto:rand_uniform(0, 300)),
                                 init_with_join_game(Owner, Host, Port, GameId, Id, Rematch, RevealMode)
                        end) || Id <- Ids ];
                        

        join_game_new ->

            {ok, GameId, _} = game_manager:create_table(game_okey, [{game_mode, color}, {sets, 2}, {rounds, 2}], Ids),

            Clients = [ proc_lib:spawn_link(fun() -> 
			         timer:sleep(crypto:rand_uniform(0, 300)),
                                 init_with_join_game(Owner, Host, Port, GameId, Id, Rematch, RevealMode)
                        end) || Id <- Ids ],

            Orders = [#bo{pid = 2, event = game_ended, event_no = 2, order = stop}];

        join_game_countdown ->
            {ok, GameId, _} = game_manager:create_table(game_okey, [{game_mode, countdown}, {gosterge_finish, true}], Ids),
            Orders = [#bo{pid = 2, event = game_ended, event_no = 10, order = stop}],
            Clients = [ proc_lib:spawn_link(fun() -> 
			      timer:sleep(crypto:rand_uniform(0, 300)),
                              init_with_join_game(Owner, Host, Port, GameId, Id, Rematch, RevealMode) end) || Id <- Ids ];

        join_game_observer ->

             L = [{true, #'TableInfo'{}}, {false, {error, <<"this_game_is_private">>}}],

            lists:map(fun({Setting, ExpectedAnswer}) ->

                    {ok, GameId, _} = game_manager:create_table(game_okey, [{observers, Setting}, 
								{game_mode, standard}, {sets, 1}, {rounds, 1}], Ids),

                    Bots = [ proc_lib:spawn_link(fun() -> 
				  timer:sleep(crypto:rand_uniform(0, 300)),
                                   init_with_join_game(Owner, Host, Port, GameId, Id, Rematch, RevealMode)
                             end) || Id <- Ids ],

                    Observers = [],  % Observers = [<<"sustel">>, <<"kate">>],

                    Bots2 = [ proc_lib:spawn_link(fun() -> 
			         timer:sleep(0), 
                                 init_with_join_game_observe(Owner, Host, Port, GameId, Id, RevealMode, ExpectedAnswer) 
                                end) || Id <- Observers ],

                   conductor([], Bots ++ Bots2)

            end, L),

            Orders = [],
            Clients = [];

        test_social_actions ->
            SenderFun = fun send_and_receive_social_action/2,
            ReceiverFun = fun receive_social_action/3,
            MyIds = [<<"radistao">>,<<"paul">>,<<"kunthar">>,<<"gleber">>],
            A = #bo{pid = 1, event = got_hand, event_no = 1, order = {do_and_continue, SenderFun, [<<"paul">>]}},
            B = #bo{pid = 2, event = got_hand, event_no = 1, order = {do_and_continue, ReceiverFun, [<<"radistao">>, <<"paul">>]}},
            C = #bo{pid = 3, event = got_hand, event_no = 1, order = {do_and_continue, ReceiverFun, [<<"radistao">>, <<"paul">>]}},
            D = #bo{pid = 4, event = got_hand, event_no = 1, order = {do_and_continue, ReceiverFun, [<<"radistao">>, <<"paul">>]}},
            Orders = [A, B, C, D],
            Clients = [ proc_lib:spawn_link(fun() -> init_with_match_me(Owner, Host, Port, Id, 0, RevealMode)
                                            end) || Id <- MyIds ];

        match_me ->
            Orders = [],
            Clients = [ proc_lib:spawn_link(fun() -> timer:sleep(crypto:rand_uniform(0, 300)),
                                                     init_with_match_me(Owner, Host, Port, Id, Rematch, RevealMode)
                                            end) || Id <- Ids ];

        match_me_disconnect ->
            DisconnectOrNot = kakamath:variate(lists:duplicate(3, true) ++
                                                   lists:duplicate(4, false)),
            IdsDisconnect = [<<"radistao">>,<<"paul">>,<<"kunthar">>,<<"gleber">>,<<"sustel">>,<<"peter">>,<<"christian">>],
            Tasks = kakamath:variate(lists:zip(DisconnectOrNot, IdsDisconnect)),
            Orders = [],
            Clients = [ proc_lib:spawn_link(fun() ->
                                  timer:sleep(crypto:rand_uniform(0, 300)),
                                  init_with_match_me_disconnect(Owner, Host, Port, Id, Rematch, RevealMode, Play)
                        end) || {Play, Id} <- Tasks ]

    end,
    conductor(Orders, Clients),
    MultiOwner ! {self(), game_ended}.

init_with_join_game(Owner, Host, Port, GameId, OwnId, Rematch, Mode) ->
    put(mode, Mode),
    log(started),
    S1 = ?TCM:connect(Host, Port),
    TT = ?TEST_TOKEN,
    #'PlayerInfo'{id = Id} = ?TCM:call_rpc(S1, #session_attach_debug{token = TT, id = OwnId}) ,
    log(connected),
    Stats = ?TCM:call_rpc(S1, #get_player_stats{game_type = <<"okey">>, player_id = Id}) ,
    #'PlayerOkeyStats'{} = Stats,
    #'TableInfo'{game = Atom} = ?TCM:call_rpc(S1, #join_game{game = GameId}) ,
    <<"okey">> = Atom,
    State = #state{conn = S1, gid = GameId, uid = Id, acker_fun = standard_acker(Owner)},
    play_set(State, Rematch),
    log(finished),
    ok = ?TCM:flush_events(?FLUSH_DELAY),
    ok = ?TCM:close(S1).

init_with_join_game_observe(_Owner, Host, Port, GameId, OwnId, Mode, EJoinResult) ->
    put(mode, Mode),
    log(started),
    S1 = ?TCM:connect(Host, Port),
    TT = ?TEST_TOKEN,
    #'PlayerInfo'{id = Id} = ?TCM:call_rpc(S1, #session_attach_debug{token = TT, id = OwnId}) ,
    log(connected),
    Stats = ?TCM:call_rpc(S1, #get_player_stats{game_type = <<"okey">>, player_id = Id}) ,
    #'PlayerOkeyStats'{} = Stats,
    JR = ?TCM:call_rpc(S1, #join_game{game = GameId}) ,
    gas:info(?MODULE,"JR: ~p, expected result: ~p", [JR, EJoinResult]),
    true = cmpr(EJoinResult, JR),
    log(finished),
    ok = ?TCM:flush_events(?FLUSH_DELAY),
    ok = ?TCM:close(S1).

init_with_match_me(Owner, Host, Port, OwnId, Rematch, Mode) ->
    put(mode, Mode),
    log(started),
    S1 = ?TCM:connect(Host, Port),
    TT = ?TEST_TOKEN,
    #'PlayerInfo'{id = Id} = ?TCM:call_rpc(S1, #session_attach_debug{token = TT, id = OwnId}) ,
    log(connected),
    Stats = tc:call_rpc(S1, #get_player_stats{game_type = <<"okey">>, player_id = Id}),
    #'PlayerOkeyStats'{level = _Level} = Stats,
    ZZZ = tc:call_rpc(S1, #match_me{game_type = <<"okey">>}) ,
    GameId =
        receive
            #'game_matched'{} = Rec->
                log(matched),
                Rec#game_matched.game
        after ?BT -> erlang:error({server_timeout, self(), ZZZ, "game_matched"})
        end,
    State = #state{conn = S1, gid = GameId, uid = Id, acker_fun = standard_acker(Owner)},
    play_set(State, Rematch).

init_with_match_me_replaceable(_Owner, Host, Port, OwnId, _Rematch, RevealMode) ->
    put(mode, RevealMode),
    log(started),
    S1 = ?TCM:connect(Host, Port),
    #'PlayerInfo'{id = Id} = ?TCM:call_rpc(S1, #session_attach_debug{token = ?TEST_TOKEN, id = OwnId}),
    log(connected),
    Stats = ?TCM:call_rpc(S1, #get_player_stats{game_type = <<"okey">>, player_id = Id}),
    #'PlayerOkeyStats'{level = _Level} = Stats,
    gas:info(?MODULE,"match me replaceable 2", []),
    ZZZ = ?TCM:call_rpc(S1, #match_me{game_type = <<"okey">>}),
    gas:info(?MODULE,"match me replaceable status: ~p", [ZZZ]),
    GameId =
        receive
            #'game_matched'{} = Rec ->
                gas:info(?MODULE,"got game_matched: ~p", [Rec]),
                true = Rec#game_matched.is_replacing,
                log(matched),
                Rec#game_matched.game
        after ?BT -> erlang:error({server_timeout, self(), ZZZ, "game_matched"})
        end,
    State = #state{conn = S1, gid = GameId, uid = Id},
    gas:info(?MODULE,"picking up game", []),
    pickup_game(State).

init_with_match_me_disconnect(Owner, Host, Port, OwnId, Rematch, Mode, false) ->
    gas:info(?MODULE,"player discon normal", []),
    init_with_match_me(Owner, Host, Port, OwnId, Rematch, Mode);
init_with_match_me_disconnect(_Owner, Host, Port, OwnId, _Rematch, _Mode, true) ->
    gas:info(?MODULE,"player discon bad", []),
    S1 = ?TCM:connect(Host, Port),
    TT = ?TEST_TOKEN,
    #'PlayerInfo'{id = Id} = ?TCM:call_rpc(S1, #session_attach_debug{token = TT, id = OwnId}) ,
    log(connected),
    Stats = ?TCM:call_rpc(S1, #get_player_stats{game_type = <<"okey">>, player_id = Id}) ,
    #'PlayerOkeyStats'{level = _Level} = Stats,
    _ZZZ = ?TCM:call_rpc(S1, #match_me{game_type = <<"okey">>}) ,
    log(disconnected),
    ok = ?TCM:flush_events(?FLUSH_DELAY),
    ok = ?TCM:close(S1).

pickup_game(S0) ->
    Id = S0#state.uid,
    gas:info(?MODULE,"ID: ~p, waiting for #okey_game_info", [Id]),
    log(game_picked_up),
    GI = receive
             #'game_event'{event = <<"okey_game_info">>, args = Args0} ->
                 A0 = api_utils:to_known_record(okey_game_info, Args0),
                 gas:info(?MODULE,"A0: ~p", [A0]),
                 A0
         after ?BT -> erlang:error({server_timeout, "game_rematched"})
         end,
    gas:info(?MODULE,"ID: ~p, waiting for #okey_game_player_state", [Id]),
    GS = receive
             #'game_event'{event = <<"okey_game_player_state">>, args = Args} ->
                 A = api_utils:to_known_record(okey_game_player_state, Args),
                 gas:info(?MODULE,"A: ~p", [A]),
                 A
         after ?BT -> erlang:error({server_timeout, "game_rematched"})
         end,
    gas:info(?MODULE,"picking up the game", []),
    NTI = GS#okey_game_player_state.next_turn_in,
    true = is_integer(NTI) orelse NTI =:= <<"infinity">>,
    SS = #'OkeySetState'{
      round_cur = GS#okey_game_player_state.current_round,
      round_max = GI#okey_game_info.rounds,
      set_cur = GI#okey_game_info.set_no,
      set_max = GI#okey_game_info.sets
     },
    Hand = GS#okey_game_player_state.tiles,
    State = S0#state{
              mode = GI#okey_game_info.game_type,
              set_state = SS,
              hand = Hand,
              gosterge = GS#okey_game_player_state.gosterge
             },
    Turn = GS#okey_game_player_state.whos_move,
    GameState = GS#okey_game_player_state.game_state,
    gas:info(?MODULE,"ID: ~p, picking up the game. Turn: ~p, GameState: ~p", [Id, Turn, GameState]),
    case {Turn, GameState} of
        {_, <<"game_finished">>} ->
            gas:info(?MODULE,"init bot finished", []),
            okey_client_rematch(State),
            play_set(State, 0);
        {_, <<"do_okey_ready">>} ->
            gas:info(?MODULE,"init bot wait", []),
            loop_and_restart(State);
        {Id, <<"do_okey_take">>} ->
            gas:info(?MODULE,"init bot: move both", []),
            State1 = do_turn(State, 1),
            okey_client_loop(State1);
        {Id, <<"do_okey_discard">>} ->
            gas:info(?MODULE,"init bot: move discard only", []),
            {TryDiscard, _} = draw_random(Hand),
            Hand1 = do_discard(State, Hand, TryDiscard, 1),
            okey_client_loop(State#state{hand = Hand1});
        {_, <<"do_okey_challenge">>} ->
            gas:info(?MODULE,"init bot: challenge", []),
            do_challenge(State),
            okey_client_loop(State#state{hand = Hand});
        {_, _} ->
            gas:info(?MODULE,"init bot: not bot's move", []),
            okey_client_loop(State#state{hand = Hand})
    end.

loop_and_restart(#state{set_state = #'OkeySetState'{round_max = MR, round_cur = RC}})
  when RC > MR, MR /= -1 ->
    ok;
loop_and_restart(State) ->
    #state{set_state = #'OkeySetState'{round_cur = RC}} = State,
    {Hand0, Gosterge0} = get_hand(State),
    gas:info(?MODULE,"Human {Hand,Gosterge}: ~p",[{Hand0,Gosterge0}]),
    LoopRes = okey_client_loop(State#state{hand = Hand0, gosterge = Gosterge0}),
    gas:info(?MODULE,"Human LoopRes: ~p",[LoopRes]),
    case LoopRes of
        <<"done">> ->
            gas:info(?MODULE,"ID: ~p, next action done", [State#state.uid]),
            ok;
        <<"next_set">> ->
            say_ready(State),
            gas:info(?MODULE,"ID: ~p, next action next_set", [State#state.uid]),
            ok;
        <<"next_round">> ->
            say_ready(State),
            gas:info(?MODULE,"ID: ~p, next action next_round", [State#state.uid]),
            State1 = State#state{set_state = #'OkeySetState'{round_cur = RC + 1}},
            check_ack(State1, game_ended, fun loop_and_restart/1, [State1])
    end.

play_set(State0, Rematch) ->
%    gas:info(?MODULE,"ID: ~p, sets: start", [State0#state.uid]),
    Id = State0#state.uid,
    State = init_okey_tour(State0),
    Rounds = (State#state.set_state)#'OkeySetState'.round_max,
    SetNo = (State#state.set_state)#'OkeySetState'.set_cur,
    Sets = (State#state.set_state)#'OkeySetState'.set_max,
    gas:info(?MODULE,"ID: ~p Start playing ~p/~p set of ~p rounds",[State0#state.uid,SetNo,Sets,Rounds]),
    loop_and_restart(State),
    gas:info(?MODULE,"ID: ~p Finish playing ~p/~p set of ~p rounds",[State0#state.uid,SetNo,Sets,Rounds]),
    case {SetNo, Rematch} of
        {X, 0} when X == Sets ->
            gas:info(?MODULE,"last set, no rematch", []),
            get_series_ended(Id),
            ok;
        {X, _} when X == Sets ->
            gas:info(?MODULE,"ID: ~p, last set, do rematch", [State0#state.uid]),
            okey_client_rematch(State),
            play_set(State, Rematch-1);
        _ ->
            gas:info(?MODULE,"ID: ~p,  play next set", [State0#state.uid]),
            play_set(State, Rematch)
    end.

init_okey_tour(State) ->
    GameId = State#state.gid,
    receive
        #'game_event'{event = <<"okey_game_info">>, args = Args, game = GI} ->
%           gas:info(?MODULE,"CLIENT TOUR STARTED okey_game_info:~n ~p", [Args]),
            GT = proplists:get_value(game_type, Args),
            Rounds = proplists:get_value(rounds, Args),
            Sets = proplists:get_value(sets, Args),
            SetNo = proplists:get_value(set_no, Args),
            true = (GI =/= <<"undefined">>),
            true = (Sets =/= <<"undefined">>),
            true = (SetNo =/= <<"undefined">>),
            true = (GT =/= <<"undefined">>),
            true = (Rounds =/= <<"undefined">>),
            true = (GI =:= GameId),
            SS = #'OkeySetState'{
              round_cur = 1,
              round_max = Rounds,
              set_cur = SetNo,
              set_max = Sets
             },
            log(game_info),
            State#state{set_state = SS, mode = GT}
    after ?BT ->
            gas:info(?MODULE,"ERROR: ~p", [{server_timeout, "game_event:okey_game_info"}]),
            erlang:error({server_timeout, "game_event:okey_game_info"})
    end.

get_series_ended(Id)->
    gas:info(?MODULE,"ID: ~p; waiting for okey_series_ended", [Id]),
    receive
        #game_event{event = <<"okey_series_ended">>} ->
            gas:info(?MODULE,"ID: ~p CLIENT SERIES ENDED", [Id]),
            log(tour_ended)
    after ?BT -> erlang:error({server_timeout, "okey_series_ended"})
    end.

okey_client_rematch(State) ->
    S1 = State#state.conn, GameId = State#state.gid, Id = State#state.uid,
    get_series_ended(Id),
    RematchR = ?TCM:call_rpc(S1, #rematch{game = GameId}) ,
    gas:info(?MODULE,"ID: ~p; rematch result: ~p", [Id, RematchR]),
    <<"ok">> = RematchR,
    receive
        #game_rematched{game = GameId} ->
            gas:info(?MODULE,"#game_rematched{game = GameId}", []),
            log(game_rematched)
    after ?BT -> erlang:error({server_timeout, "game_rematched"})
    end.

say_ready(State) ->
    S1 = State#state.conn,
    GameId = State#state.gid,
    <<"ok">> = ?TCM:call_rpc(S1, #game_action{game = GameId, action = okey_ready, args = []}) .

get_hand(State) ->
    S1 = State#state.conn,
    GameId = State#state.gid,
    receive
        #'game_event'{event = <<"okey_game_started">>, args = Args} = _Msg ->
            log(game_started),
            MH = proplists:get_value(tiles, Args),
            G = proplists:get_value(gosterge, Args),
            CR = proplists:get_value(current_round, Args),
            CS = proplists:get_value(current_set, Args),

            ((MH == undefined) orelse (G == undefined)) andalso erlang:error(cant_get_params_of_hand),

            gas:info(?MODULE,"Human Round/Set: ~p/~p", [CR,CS]),
            HasGosterge = lists:member(G, MH),
            HasGostergeServer = ?TCM:call_rpc(S1, #game_action{game = GameId, action = okey_has_gosterge, args = []}),
            gas:info(?MODULE,"HasGostergeServer: ~p",[HasGostergeServer]),
            HasGosterge = HasGostergeServer,
            check_ack(State, got_hand, fun() -> ok end, []),
            {MH, G}
    after ?BT ->
            erlang:error({server_timeout, "game_event:okey_game_started"})
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

validate_hand(S, GameId, Hand) ->
    case ?TCM:call_rpc(S, #game_action{
                         game = GameId,
                         action = okey_debug,
                         args = []})  of
        {error, Reason} ->
            gas:info(?MODULE,"dying in fire. Reason: ~p", [Reason]),
            erlang:error(die_in_fire);
        ServerHand ->
            Res = game_okey:is_same_hand(ServerHand, Hand),
            case Res of
                false ->
                    gas:info(?MODULE,"validation failed: ~p =/= ~p", [Hand, ServerHand]);
                _ ->
                    ok
            end,
            Res
    end.

okey_client_loop(State) ->
    Id = State#state.uid,
    receive
        #'game_event'{event = <<"okey_next_turn">>, args = Args} ->
            Timeout = crypto:rand_uniform(0, ?SIM_DELAY),
            State1 = case {proplists:get_value(player, Args), proplists:get_value(can_challenge, Args)} of
                         {Id, false} ->
                             do_turn(State, Timeout);
                         {_OtherId, _} ->
                             State
                     end,
            okey_client_loop(State1);
        #'game_event'{event = <<"okey_player_ready">>} ->
            okey_client_loop(State);
        #'game_event'{event = <<"okey_player_has_gosterge">>, args = _Args} ->
            okey_client_loop(State);
        #'game_event'{event = <<"okey_tile_taken">>, args = Args} ->
            case proplists:get_value(revealed, Args) of
                null ->
                    NewH = proplists:get_value(pile_height, Args),
                    okey_client_loop(State#state{pileh = NewH});
                _ ->
                    okey_client_loop(State)
            end;
        #'game_event'{event = <<"okey_tile_discarded">>} ->
            okey_client_loop(State);
        #'game_event'{event = <<"okey_revealed">>} ->
            do_challenge(State),
            okey_client_loop(State);
        #'game_event'{event = <<"okey_series_ended">>, args = Args} ->
            S = State#state.conn,
            ?TCM:rpc(S, #logout{}),
            NextAction = proplists:get_value(next_action, Args),
            NextAction;
        #'game_event'{event = <<"okey_round_ended">>, args = Args} ->
            okey_round_ended_checks(Args, State),
            GS = proplists:get_value(good_shot, Args),
            Reason = proplists:get_value(reason, Args),
            NextAction = proplists:get_value(next_action, Args),
            gas:info(?MODULE,"ID: ~p game ended, good_shot: ~p, reason: ~p", [Id, GS, Reason]),
            NextAction;
        #player_left{} ->
            okey_client_loop(State);
        #'game_event'{event = <<"game_rematched">>, args = _Args} ->
            okey_client_loop(State);
        #'game_event'{event = <<"okey_game_started">>, args = _Args} ->
            okey_client_loop(State);
%            erlang:error({protocol_breach, okey_series_ended});
        #'game_event'{event = <<"okey_game_info">>, args = _Args} ->
            okey_client_loop(State);
%            erlang:error({protocol_breach, okey_game_info});
        #'game_event'{event = <<"player_left">>} ->
            okey_client_loop(State);
        #'game_event'{args = Args} ->
            NextAction = proplists:get_value(next_action, Args),
            NextAction;
        _Msg ->
            gas:info(?MODULE,"the msg: ~p", [_Msg]),
            erlang:error({bot_received_unrecognized_message, _Msg})
    after ?BT ->
            log(server_timeouted),
            erlang:error({server_timeout, "okey_client_loop_timeout"})
    end.

do_turn(State, Timeout) ->
    {Timeouted, Hand1} = do_take(State, Timeout),
    true = is_list(Hand1),
    {TryDiscard, WinningHand} = draw_random(Hand1),
    Pile0Height = State#state.pileh,
    FHand = case {Timeouted, is_revealing(Pile0Height)} of
                {false, true} ->
                    do_reveal(State, WinningHand, TryDiscard);
                {false, false} ->
                            do_discard(State, Hand1, TryDiscard, Timeout);
                {true, _} ->
                    Hand1
            end,
    State#state{hand = FHand}.

do_challenge(State) ->
    GameId = State#state.gid,
    S = State#state.conn,
    ZZZ = ?TCM:call_rpc(S, #game_action{
                         game = GameId,
                         action = okey_challenge,
                         args = [ {challenge, random_bool(0.2)} ]}),
    gas:info(?MODULE,"ID: ~p challenge result: ~p", [State#state.uid, ZZZ]),
    ok.

do_take(State, Timeout) ->
    S = State#state.conn,
    GameId = State#state.gid,
    Hand = State#state.hand,
    {is_list, true} = {is_list, is_list(Hand)},
    receive
        #'game_event'{event = <<"okey_turn_timeout">>, args = Args} ->
                                                % gas:info(?MODULE,"ID: ~p I timeouted on take", [Id]),
            TileT = proplists:get_value(<<"tile_taken">>, Args),
            TileD = proplists:get_value(<<"tile_discarded">>, Args),
            Hand1 = lists:delete(TileD, Hand),
            {true, [TileT | Hand1]}
    after Timeout ->
            Pile = crypto:rand_uniform(0, 2),
            case ?TCM:call_rpc(S, #game_action{
                               game = GameId,
                               action = okey_take,
                               args = [ {pile, Pile} ]}) of
                #'OkeyPiece'{} = Tosh ->
                    MyHand = [Tosh | Hand],
                                                % gas:info(?MODULE,"ID: ~p Take tosh in ~p pile! Get: ~p", [Id, Pile, Tosh]),
                    {false, MyHand};
                {error, <<"cant_take_do_discard">>} ->
                                                % gas:info(?MODULE,"ID: ~p Has 15 items in hand. 15=~p", [Id, length(Hand)]),
                    {false, Hand};
                {error, <<"game_has_already_ended">>} = Err ->
                    case State#state.mode of
                        <<"countdown">> ->
                            {false, Hand};
                        _ ->
                            gas:info(?MODULE,"ID: ~p; mode:~p; failed take with msg ~p",
                                [State#state.uid, State#state.mode, Err]),
                            erlang:error(failed_take)
                    end;
                Err ->
                    gas:info(?MODULE,"ID: ~p failed take with msg ~p", [State#state.uid, Err]),
                    erlang:error(failed_take)
            end
    end.

do_discard(State, Hand, Item, Timeout) ->
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    receive
        #'game_event'{event = <<"okey_tile_discarded">>, args = Args} ->
            case proplists:get_value(<<"player">>, Args) of
                Id ->
                    Tile = proplists:get_value(<<"player">>, Args),
                    lists:delete(Tile, Hand)
            end
    after Timeout ->
            Hand1 = lists:delete(Item, Hand),
            _Res = ?TCM:call_rpc(S, #game_action{game = GameId, action = <<"okey_discard">>,
                                               args = [ {tile, Item} ]}),
            Hand1
    end.

do_reveal(State, Hand, Item) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ?TCM:call_rpc(S, #game_action{
                    game = GameId,
                    action = okey_reveal,
                    args = [ {discarded, Item},
                             {hand, Hand}
                            ]}),
    Hand.

draw_random([One]) ->
    {One, []};
draw_random(List) ->
    {is_list, true} = {is_list, is_list(List)},
    Pos = crypto:rand_uniform(1, length(List)),
    Joker = lists:nth(Pos, List),
    ResList = lists:delete(Joker, List),
    {Joker, ResList}.

is_revealing(undefined) ->
    false;
is_revealing(PileHeight) ->
    Mode = case get(mode) of
               undefined -> normal;
               A -> A
           end,
    MaxProb = case get(reveal_probability) of
                  undefined -> 700.0;
                  Value -> Value
              end,
    is_revealing(PileHeight, MaxProb, Mode).

is_revealing(PileHeight, MaxProb, normal) ->
    HS = game_okey:hand_size(),
    MaxHeight = ((HS - 1) * 4 * 2 + 2.0) - (HS * 4),
    X = erlang:abs((MaxHeight - PileHeight) / MaxHeight),
    Prob = (X * X * X * X * X * X) * MaxProb,
    Point = crypto:rand_uniform(1, 1000),
    Prob > Point;
is_revealing(_PileHeight, _, empty_pile) ->
    false.

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

%% checks
okey_round_ended_checks(Args, State) ->
    Score = proplists:get_value(score, Args),
    Mode = State#state.mode,
    {newer_go_below_10, true} =
        {newer_go_below_10, Mode =/= <<"countdown">> orelse Score > -1}.

%% Tests
reveal_probability_loop(0) ->
    erlang:error(reveal_probability_loop_failed);
reveal_probability_loop(N) ->
    case is_revealing(N) of
        true ->
            ok;
        false ->
            reveal_probability_loop(N-1)
    end.
reveal_probability_loop_test() ->
    put(reveal_probability, 1000.0),
    reveal_probability_loop(50).
log(Msg) ->
    ?TCM:log(Msg).

random_bool(Prob) ->
    Point = crypto:rand_uniform(0, 1000),
    Prob*1000 > Point.

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

send_and_receive_social_action(State, Recipient) ->
    GID = State#state.gid,
    ?TCM:call_rpc(State#state.conn, #social_action{game = GID, type = 0, recipient = Recipient}),
    receive_social_action(State, State#state.uid, Recipient).

receive_social_action(_State, Sender, Recipient) ->
    receive
        #social_action_msg{type = Type, initiator = I, recipient = R} ->
            true = Type == 0,
            true = Sender == I,
            true = Recipient == R
    end.

cmpr(X, Y) when element(1, X) == element(1, Y) ->
    true;
cmpr(B, B) ->
    true;
cmpr(_, _) ->
    false.


