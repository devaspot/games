-module(tavla_bot).
-author('Maxim Sokhatsky <maxim@synrc.com>').
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).

-include_lib("server/include/basic_types.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/game_tavla.hrl").
-include_lib("server/include/game_okey.hrl").
-include_lib("server/include/settings.hrl").

-record(state, {
        moves = 0 :: integer(),
%%        started = false :: boolean(),
%%        next_initiated = false :: boolean(),
        table_id :: integer(),
        is_robot = true :: boolean(),
        board :: list(tuple('Color'(), integer) | null),
        user :: #'PlayerInfo'{},
        player_color :: integer(),
        players,
        uid :: 'PlayerId'(),
        owner :: pid(),
        owner_mon,
        session :: pid(),
        gid :: 'GameId'(),
        bot :: pid(),
        conn :: pid(),
        hand :: list(),
        running_requests = dict:new() :: any(),
        delay :: integer(),
        mode :: atom(),
        request_id = 0,
        confirmation  :: yes_exit | no_exit | no}).

% gen_server

send_message(Pid, Message) -> gen_server:call(Pid, {server, Message}).
call_rpc(Pid, Message) -> gen_server:call(Pid, {call_rpc, Message}).
get_session(Pid) -> gen_server:call(Pid, get_session).
init_state(Pid, Situation) -> gen_server:cast(Pid, {init_state, Situation}).
join_game(Pid) -> gen_server:cast(Pid, join_game).
start(Owner, PlayerInfo, GameId) -> gen_server:start(?MODULE, [Owner, PlayerInfo, GameId], []).
start_link(Owner, PlayerInfo, GameId) -> gen_server:start_link(?MODULE, [Owner, PlayerInfo, GameId], []).

init([Owner, PlayerInfo, GameId]) ->
    {ok, SPid} = game_session:start_link(self()),
    game_session:bot_session_attach(SPid, PlayerInfo),
    UId = PlayerInfo#'PlayerInfo'.id,
    gas:info(?MODULE,"BOTMODULE ~p started with game_session pid ~p", [UId,SPid]),
    {ok, #state{user = PlayerInfo, uid = UId, owner = Owner, gid = GameId, session = SPid}}.


handle_call({server, Msg0}, _From, State) ->
    BPid = State#state.bot,
    Msg = flashify(Msg0),
    BPid ! Msg,
    {reply, ok, State};

handle_call({call_rpc, Msg}, From, State) ->
    RR = State#state.running_requests,
    Id = State#state.request_id + 1,
    Self = self(),
    RR1 = dict:store(Id, From, RR),
    proc_lib:spawn_link(fun() ->
        Res = try
                  Answer = game_session:process_request(State#state.session, "TAVLA BOT", Msg),
                  {reply, Id, Answer}
              catch
                  _Err:Reason -> {reply, Id, {error, Reason}}
              end,
        gen_server:call(Self, Res)
    end),
    {noreply, State#state{running_requests = RR1, request_id = Id}};

handle_call({reply, Id, Answer}, _From, State) ->
    RR = State#state.running_requests,
    From = dict:fetch(Id, RR),
    gen_server:reply(From, Answer),
    {reply, ok, State};

handle_call(get_session, _From, State) ->
    {reply, State#state.session, State};
%    {ok, SPid} = game_session:start_link(self()),
%    game_session:bot_session_attach(SPid, State#state.user),
%    {reply, State#state.session, State#state{session = SPid}};

handle_call(Request, _From, State) ->
    Reply = ok,
    gas:info(?MODULE,"unknown call: ~p", [Request]),
    {reply, Reply, State}.

handle_cast(join_game, State) ->
    Mon = erlang:monitor(process, State#state.owner),
    UId = State#state.uid,
    GId = State#state.gid,
    gas:info(?MODULE,"Init State User ~p",[State#state.user]),
    BPid = proc_lib:spawn_link(tavla_bot, robot_init, [#state{gid = GId, uid = UId, conn = self(), table_id = State#state.table_id, user = State#state.user}]),
    BPid ! join_game,
    {noreply, State#state{bot = BPid, owner_mon = Mon}};

handle_cast(Msg, State) ->
    gas:info(?MODULE,"unknown cast: ~p", [Msg]),
    {noreply, State}.

handle_info({'DOWN', Ref, process, _, Reason}, State = #state{owner_mon = OMon}) when OMon == Ref ->
    gas:info(?MODULE,"relay goes down with reason ~p so does bot", [Reason]),
    {stop, Reason, State};
handle_info({server,M}, State) ->
    BPid = State#state.bot,
    BPid ! M,
    {noreply, State};
handle_info(Info, State) ->
    {stop, {unrecognized_info, Info}, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

% loops

robot_init(State) ->
    gas:info(?MODULE,"Robot Init User Info ~p",[State#state.user]),
    robot_init_loop(State).

robot_init_loop(State) -> % receiving messages from relay
    S = State#state.conn,
    Id = State#state.uid,
    GameId = State#state.gid,
    receive
        join_game ->
            case call_rpc(S, #join_game{game = GameId}) of
                ok -> tavla_client_loop(State);
                _Err -> gas:info(?MODULE,"ID: ~p failed take with msg ~p", [Id, _Err]),
                        erlang:error(robot_cant_join_game)
            end
    end.

tavla_client_loop(State) -> % incapsulate tavla protocol
    timer:sleep(300),
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    MyColor = State#state.player_color,
    GameMode = State#state.mode,
    receive
        #game_event{event = <<"tavla_next_turn">>, args = Params} = Msg ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            case check_can_roll(GameMode) of
                true ->
                    case fix_color(proplists:get_value(color, Params)) of
                        MyColor ->
                            gas:info(?MODULE,"TAVLABOT ~p Doing roll", [Id]),
                            roll_action(State,TableId),
                            tavla_client_loop(State);
                        _  -> tavla_client_loop(State)
                    end;
                false ->
                    gas:info(?MODULE,"TAVLABOT ~p Ignoring tavla_next_turn (paired mode). Server rolls automaticly.", [Id]),
                    tavla_client_loop(State)
            end;
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_moves">>, args = Params} = Msg ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            PlayerColor = fix_color(proplists:get_value(color, Params)),
            From = proplists:get_value(from, Params),
            To = proplists:get_value(to, Params),
%%            gas:info(?MODULE,"board before moves: ~p", [State#state.board]),
            Board = reverse_board(State#state.board, PlayerColor),
            FromR=rel(From,PlayerColor), ToR=rel(To,PlayerColor),
            NewBoard = reverse_board(follow_board(Board,FromR,ToR,PlayerColor), PlayerColor),
%%            gas:info(?MODULE,"board after moves: ~p", [NewBoard]),
            tavla_client_loop(State#state{board = NewBoard,moves = State#state.moves + 1});

            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_turn_timeout">>, args = Params} = Msg ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            PlayerColor = fix_color(proplists:get_value(color, Params)),
            Moves = ext_to_moves(proplists:get_value(moves, Params)),
%%            gas:info(?MODULE,"board before moves: ~p", [State#state.board]),
            Board = reverse_board(State#state.board, PlayerColor),
            F = fun({From, To}, B) ->
                        FromR = rel(From, PlayerColor), ToR = rel(To, PlayerColor),
                        follow_board(B, FromR, ToR, PlayerColor)
                end,
            NewBoard = reverse_board(lists:foldl(F, Board, Moves), PlayerColor),
%%            gas:info(?MODULE,"board after moves: ~p", [NewBoard]),
            tavla_client_loop(State#state{board = NewBoard, moves = State#state.moves + length(Moves)});
            _ -> tavla_client_loop(State) end;
%%         #game_event{event = <<"tavla_vido_request">>, args = Params} ->
%%             TableId = proplists:get_value(table_id, Params, 0),
%%             case TableId == State#state.table_id of true ->
%% 
%% %            gas:info(?MODULE,"tavla moves: ~p",[Params]),
%%             To = proplists:get_value(from, Params),
%%             vido(State,To,TableId),
%%             tavla_client_loop(State);
%% 
%%             _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_surrender_request">>, args = Params} = Msg->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),

%            gas:info(?MODULE,"tavla moves: ~p",[Params]),
            To = proplists:get_value(from, Params),
            surrender(State,To,TableId),
            tavla_client_loop(State);

            _ -> tavla_client_loop(State) end;
%%         #game_event{event = <<"tavla_ack">>, args = Params} ->
%%             TableId = proplists:get_value(table_id, Params, 0),
%%             case TableId == State#state.table_id of true ->
%%             _To = proplists:get_value(from, Params),
%%             _Type = proplists:get_value(type, Params),
%%             tavla_client_loop(State);
%% 
%%             _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_player_state">>, args = Params} = Msg->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            PlayersColors = proplists:get_value(players_colors, Params),
            WhosMove = proplists:get_value(whos_move, Params),
            BoardRaw = proplists:get_value(board, Params),
            Dice = proplists:get_value(dice, Params),
            GameState = proplists:get_value(game_state, Params),
            Paused = proplists:get_value(paused, Params),

            case Paused of
                 true -> wait_for_resume();
                 false -> ok
            end,

            FoundMyself = lists:keyfind(Id, #tavla_color_info.name, PlayersColors),
            PlayerColor = fix_color(FoundMyself#tavla_color_info.color),
            gas:info(?MODULE,"TAVLABOT ~p BoardRaw: ~p", [Id, BoardRaw]),
            Board = if BoardRaw == null -> null;
                       true -> ext_to_board(BoardRaw)
                    end,
            State1 = State#state{board = Board, moves = 0, player_color=PlayerColor},

            gas:info(?MODULE,"TAVLABOT ~p player color: ~p",[Id, PlayerColor]),
            MyMove = lists:member(fix_color(PlayerColor), WhosMove),
            case {MyMove, GameState} of
                {true, <<"first_move_competition">>} ->
                    roll_action(State1, TableId),
                    tavla_client_loop(State1);
                {true, <<"waiting_for_roll">>} ->
                    roll_action(State1, TableId),
                    tavla_client_loop(State1);
                {true, <<"waiting_for_move">>} ->
                    do_move(State1, Dice, TableId, PlayerColor),
                    tavla_client_loop(State1#state{moves = 1});
                {_, <<"initializing">>} ->
                    tavla_client_loop(State1);
                {_, <<"finished">>} ->
                    tavla_client_loop(State1);
                {false, _} ->
                    tavla_client_loop(State1)
            end;
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_started">>, args = Params} = Msg->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            Players = proplists:get_value(players, Params),
            BoardRaw = proplists:get_value(board, Params),
            Competition = proplists:get_value(do_first_move_competition_roll, Params),
            FoundMyself = lists:keyfind(Id, #tavla_color_info.name, Players),
            PlayerColor = fix_color(FoundMyself#tavla_color_info.color),
            gas:info(?MODULE,"TAVLABOT ~p game_started, color: ~p",[Id, PlayerColor]),
            gas:info(?MODULE,"TAVLABOT ~p game_started, BoardRaw: ~p",[Id, BoardRaw]),
            Board = if BoardRaw == null -> null;
                       true -> ext_to_board(BoardRaw)
                    end,
            if Competition -> roll_action(State,TableId); true -> do_nothing end,
            tavla_client_loop(State#state{board = Board, moves=0, player_color=PlayerColor});
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_won_first_move">>, args = Params} = Msg ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            State2 = case fix_color(proplists:get_value(color, Params)) of
                MyColor ->
                    gas:info(?MODULE,"TAVLABOT ~p : I won the first move order.", [Id]),
                    case proplists:get_value(reroll, Params) of
                        false ->
                            Dice = proplists:get_value(dice, Params),
                            gas:info(?MODULE,"TAVLABOT ~p Reroll is not needed. Doing the first move with dice:~p",[Id, Dice]),
                            do_move(State,Dice,TableId,MyColor), State#state{moves = State#state.moves + 1};
                        true ->
                            gas:info(?MODULE,"TAVLABOT ~p Reroll needed. So doing it.",[Id]),
                            roll_action(State, TableId)
                    end;
                _  -> gas:info(?MODULE,"TAVLABOT ~p tavla_won_first_move: Ignore rolls (another color)",[Id]), State
            end,
            tavla_client_loop(State2);
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_rolls">>, args = Params} = Msg->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            State2 = case fix_color(proplists:get_value(color, Params)) of
                MyColor ->
                      Dice = proplists:get_value(dices, Params),
                      case Dice of
                        [_A,_B] ->
                            gas:info(?MODULE,"TAVLABOT ~p Doing moves with dice: ~p",[Id, Dice]),
                            do_move(State,Dice,TableId,MyColor), State#state{moves = State#state.moves + 1};
                        [_C] ->
                            gas:info(?MODULE,"TAVLABOT ~p Ignore the roll (first move competition)",[Id]),
                            State
                      end;
                _  -> gas:info(?MODULE,"TAVLABOT ~p Ignore the roll (another color)",[Id]), State
            end,
            tavla_client_loop(State2);
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"player_left">>, args = Params} = Msg ->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            Replaced = proplists:get_value(bot_replaced, Params, false) orelse
                       proplists:get_value(human_replaced, Params, false),
            case Replaced of
                false ->
                    call_rpc(S, #logout{});
                true ->
                    tavla_client_loop(State)
            end;
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_info">>, args = Args} = Msg->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
             User = State#state.user,
             Mode = proplists:get_value(game_mode, Args),
             TableId = proplists:get_value(table_id, Args),
             gas:info(?MODULE,"TAVLABOT game_info ~p ~p",[self(),User]),
             Players = proplists:get_value(players, Args),
             SeriesConfirmMode = proplists:get_value(series_confirmation_mode, Args),
             gas:info(?MODULE,"TAVLABOT players: ~p",[Players]),
             Delay = get_delay(fast),
             CatchTID = case User == undefined of
                            false -> FoundMyself = lists:keyfind(User#'PlayerInfo'.id,#'PlayerInfo'.id,Players),
                                    case FoundMyself of false -> State#state.table_id; _ -> TableId end;
                            true -> gas:info(?MODULE,"ERROR USER in ~p is not set!",[self()]), undefined
                        end,
             tavla_client_loop( State#state{table_id = CatchTID, delay = Delay, mode = Mode,
                                            players = Players, confirmation = SeriesConfirmMode});
        #game_event{event = <<"tavla_series_ended">>, args = Params} = Msg->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            tavla_client_loop(State);
            _ -> tavla_client_loop(State) end;
        #game_event{event = <<"tavla_game_ended">>, args = Params} = Msg->
            TableId = proplists:get_value(table_id, Params, 0),
            case TableId == State#state.table_id of true ->
            gas:info(?MODULE,"TAVLABOT ~p : Received message: ~p", [Id, Msg]),
            say_ready(State,TableId),
            tavla_client_loop(State);
            _ -> tavla_client_loop(State) end;
        Msg ->
            gas:info(?MODULE,"TAVLABOT ~p Received UNKNOWN message: ~p",[Id, Msg]),
            tavla_client_loop(State)
    end.

fix_color(Color) ->  case Color of 1 -> 2; 2 -> 1 end.



check_can_roll(Mode) ->
    case Mode of
        <<"paired">> -> false;
        _ -> true
    end.



% logic

follow_board(Board,Moves,PlayerColor) ->
    lists:foldl(fun ({From,To}, Acc) -> follow_board(Acc,From,To,PlayerColor) end, Board, Moves).

follow_board(Board,From,To,PlayerColor) -> % track the moves to keep board consistent
    FromCell = lists:nth(From + 1,Board),
    {FromColor,_FromCount} = case FromCell of 
                                 null -> {0,0};
                                 _Else -> _Else
                            end,
    BoardWithKicks = [ case No of
          From -> case Cell of
                       {_Color,1} -> {{0,0},null};
                       {Color,Count} -> {{0,0},{Color,Count-1}};
                       _ -> gas:info(?MODULE,"Board: ~p From ~p To ~p",[Board,From,To]),
                            gas:info(?MODULE,"follow_board: cant move from empty slot"), exit(self(),kill), {{0,0},null}
                  end;
            To -> case Cell of
                       null -> case FromColor of 0 -> {{0,0},null}; _ -> {{0,0},{FromColor,1}} end;
                       {0,0} -> case FromColor of 0 -> {{0,0},null}; _ -> {{0,0},{FromColor,1}} end;
                       {Color,Count} -> 
                            case Color =:= FromColor of
                                 true -> {{0,0},{Color,Count+1}};
                                 false -> case Count of
                                               1 when FromColor =/= 0 -> {{Color,1},{FromColor,1}};
                                               _ -> gas:info(?MODULE,"Board: ~p From ~p To ~p",[Board,From,To]),
                                                    gas:info(?MODULE,"follow_board: cant kick tower"), exit(self(),kill), {{0,0},{Color,Count}}
                                          end
                            end
                  end;
           _ -> {{0,0},Cell}
    end || {Cell,No} <- lists:zip(Board,lists:seq(0,27)) ],
    {KickColor,KickAmount} = lists:foldl(
            fun({{KC,KA},_},{Col,Sum}) ->
                  case KC of
                       0 -> {Col,Sum};
                       _ -> {KC,Sum+KA} 
                  end
            end,{0,0}, BoardWithKicks),
%%    gas:info(?MODULE,"Kick: ~p",[{KickColor,KickAmount}]),
    NewBoard = [ case {No,KickColor} of
        {25,_} when KickColor =/= 0 -> case Cell of
                       null -> {KickColor,KickAmount};
                       {_Color,Sum} -> {KickColor,KickAmount+Sum}
                  end;
        _ -> Cell
    end || {{{_KC,_KA},Cell},No} <- lists:zip(BoardWithKicks,lists:seq(0,27))],
    NewBoard.

all_in_home(Board,Color) ->
    Lates = lists:foldr(fun (A,Acc) -> 
                              case A of
                                   {null,_No} -> Acc;
                                   {{C,_Count},No} -> NotInHome = (((No > 0)and(No < 19)) or (No == 26)),
                                       case {C, NotInHome} of
                                            {Color,true} -> Acc + 1;
                                            _ -> Acc
                                       end
                              end
                          end,0,lists:zip(Board,lists:seq(0,27))),
    Lates =:= 0.

make_decision(Board,Dices2,Color,TableId) -> % produces tavla moves
    [X,Y] = Dices2,
    Dices = case X =:= Y of true -> [X,X,X,X]; false -> Dices2 end,
    Decision = first_available_move(Board,Dices,Color,TableId),
    gas:info(?MODULE,"Decision: ~p",[Decision]),
    Decision.

%% norm([A,B]) -> case A > B of true -> {A,B}; false -> {B,A} end.
first_move_table() -> [{{6,6},[{13,7},{13,7},{24,18},{24,18}]}, % based on 
                       {{6,5},[{24,13}]},
                       {{6,4},[{24,18},{13,9}]},
                       {{6,3},[{24,18},{13,10}]},
                       {{6,2},[{24,18},{13,11}]},
                       {{6,1},[{13,7},{8,7}]},
                       {{5,5},[{13,3},{13,3}]},
                       {{5,4},[{24,20},{13,8}]},
                       {{5,3},[{8,3},{5,3}]},
                       {{5,2},[{24,22},{13,8}]},
                       {{5,1},[{24,23},{13,8}]},
                       {{4,4},[{24,20},{24,20},{13,9},{13,9}]},
                       {{4,3},[{24,21},{13,9}]},
                       {{4,2},[{8,4},{6,4}]},
                       {{4,1},[{24,23},{13,9}]},
                       {{3,3},[{24,21},{24,21},{13,10},{13,10}]},
                       {{3,2},[{24,21},{13,11}]},
                       {{3,1},[{8,5},{6,5}]},
                       {{2,2},[{13,11},{13,11},{6,4},{6,4}]},
                       {{2,1},[{13,11},{6,5}]},
                       {{1,1},[{8,7},{8,7},{6,5},{6,5}]}
                      ].

%make_first_move(Dices,TableId,PlayerColor) -> 
%    {_,Moves} = lists:keyfind(norm(Dices),1,first_move_table()),
%    case PlayerColor of
%         1 -> [ #'TavlaAtomicMove'{table_id = TableId,from=25-From,to=25-To} || {From,To} <- Moves];
%         2 -> [ #'TavlaAtomicMove'{table_id = TableId,from=From,to=To} || {From,To} <- Moves]
%    end.

tactical_criteria(Board,Color) -> 
   case all_in_home(Board,Color) of
        true  -> finish;
        false -> case lists:nth(27,Board) of
                      {Color,Count} -> kicks;
                      _ -> race
                 end
   end.

rel(X,C) -> case C of 2 -> case X of 0->27;27->0;26->25;25->26;_->25-X end; 
                      1 -> X end.

reverse_board(Board,PlayerColor) ->
    case PlayerColor of
         1 -> Board;
         2 -> [BE|Rest] = Board, [WE,WK,BK|Rest1] = lists:reverse(Rest), [WE] ++ Rest1 ++ [WK,BK,BE]
    end.

%% first_available_move(RealBoard,Dice,Color,TableId) -> Moves
first_available_move(RealBoard,Dice,Color,TableId) ->
    RelativeBoard = reverse_board(RealBoard,Color),
    F = fun(Die, {MovesAcc, BoardAcc, FailedDiceAcc}) ->
%%                gas:info(?MODULE,"board: ~p", [BoardAcc]),
                Tactic = tactical_criteria(BoardAcc, Color),
%%                gas:info(?MODULE,"tactical criteria: ~p", [Tactic]),
                case find_move(Color, Die, Tactic, BoardAcc) of
                    {Move, NewBoard} -> {[Move | MovesAcc], NewBoard, FailedDiceAcc};
                    false -> {MovesAcc, BoardAcc, [Die | FailedDiceAcc]}
                end
        end,
    {List, Board, FailedDice} = lists:foldl(F, {[], RelativeBoard, []}, Dice),
%%    gas:info(?MODULE,"moves found: ~p",[{List, FailedDice}]),
    [#'TavlaAtomicMove'{from=rel(From,Color), to=rel(To,Color)} || {From,To} <- lists:reverse(List)]  ++
        if length(FailedDice) == 0; length(FailedDice) == length(Dice) -> [];
           true -> first_available_move(reverse_board(Board,Color),FailedDice,Color,TableId) end.

%% find_move(Die, Tactic, Board) -> {Move, NewBoard} | false
find_move(Color, Die, Tactic, Board) ->
    OppColor = case Color of 1 -> 2; 2 -> 1 end,
    find_move(Color, OppColor, Die, Tactic, Board, lists:sublist(lists:zip(Board,lists:seq(0,27)),2,24)).

find_move(_Color, _OppColor, _Die, _Tactic, _Board, []) -> false;
find_move(Color, OppColor, Die, Tactic, Board, [{Cell, No} | Rest]) ->
%%    gas:info(?MODULE,"checking pos: ~p", [{Cell, No, Die, Tactic}]),
    case {Cell, Tactic} of
        {null,kicks} when No == Die -> gas:info(?MODULE,"found kick to empty: ~p",[{26,No}]), {{26,No}, follow_board(Board,26,No,Color)};
        {{OppColor,1},kicks} when No == Die -> gas:info(?MODULE,"found kick kick: ~p",[{26,No}]), {{26,No}, follow_board(Board,26,No,Color)};
        {{Color,_},kicks} when No == Die -> gas:info(?MODULE,"found kick over own: ~p",[{26,No}]), {{26,No}, follow_board(Board,26,No,Color)};
        {{Color,_},finish} when No + Die >= 25 -> gas:info(?MODULE,"found finish move: ~p",[{No,27}]), {{No,27}, follow_board(Board,No,27,Color)};
        {{Color,_},Tactic} when (Tactic==race orelse Tactic==finish) andalso (No + Die < 25) ->
            case lists:nth(No+Die+1, Board) of
                null -> gas:info(?MODULE,"found race to empty: ~p",[{No,No+Die}]), {{No,No+Die}, follow_board(Board,No,No+Die,Color)};
                {Color,_} -> gas:info(?MODULE,"found race over own: ~p",[{No,No+Die}]), {{No,No+Die}, follow_board(Board,No,No+Die,Color)};
                {OppColor,1} -> gas:info(?MODULE,"found race kick: ~p",[{No,No+Die}]), {{No,No+Die}, follow_board(Board,No,No+Die,Color)};
                _ -> find_move(Color, OppColor, Die, Tactic, Board, Rest)
            end;
        _ -> find_move(Color, OppColor, Die, Tactic, Board, Rest)
    end.

%% make_end_series(Color,TableId,Board) ->
%%     lists:foldl(fun (A,Acc) -> gas:info(?MODULE,"~p / ~p~n",[A,Acc]), {Cell,No} = A, case Cell of null -> Acc; 
%%                                   {Color,Count} -> Acc ++
%%     [ #'TavlaAtomicMove'{from=No, to=27} || X <- lists:seq(1,Count)]; _ -> Acc end
%%                                  end, [], lists:sublist(lists:zip(Board,lists:seq(0,27)),1,27)  ).

% actions

say_ready(State,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, action = tavla_ready, args = [{table_id,TableId}]}).

vido(State,To,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, 
                                  action = tavla_vido_answer,
                                  args = [{table_id,TableId},{from,State#state.uid},{to,To},{answer,false}]}).

surrender(State,To,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, 
                                  action = tavla_surrender_answer,
                                  args = [{table_id,TableId},{from,State#state.uid},{to,To},{answer,true}]}).

roll_action(State,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    ok = call_rpc(S, #game_action{game = GameId, action = tavla_roll, args = [{table_id,TableId}]}).

do_skip(State,TableId) ->
    S = State#state.conn,
    GameId = State#state.gid,
    call_rpc(S, #game_action{
                        game = GameId,
                        action = tavla_skip,
                        args = [{table_id,TableId}]}).

do_move(State, Dices,TableId,PlayerColor) ->
    Delay = State#state.delay,
    simulate_delay(take, Delay),
    S = State#state.conn,
    GameId = State#state.gid,
    Id = State#state.uid,
    
%    Decision = case State#state.moves > 5 of 
%                    false -> make_decision(State#state.board, Dices, PlayerColor,TableId);
%                    true -> make_end_series(PlayerColor,TableId,State#state.board)
%               end,
    Decision = make_decision(State#state.board, Dices, PlayerColor,TableId),
    case Decision of
        [] ->
            gas:info(?MODULE,"TAVLABOT ~p : No moves available. Do nothing", [Id]),
            no_moves;
        _ ->
            gas:info(?MODULE,"TAVLABOT ~p : Moves: ~p", [Id, Decision]),
            Resp = call_rpc(S, #game_action{game = GameId,
                                            action = tavla_move,
                                            args = [{table_id, TableId},{player, Id},{moves, Decision }]}),
            gas:info(?MODULE,"TAVLABOT_DBG ~p : Server response: ~p", [Id, Resp]),
            {ok, Decision, Resp}
    end.

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

time_to_sleep(_, Delay) ->
    erlang:trunc((Delay / 3) * 2).

simulate_delay(Action, Delay) ->
    TheDelay = time_to_sleep(Action, Delay),
    receive
        #game_paused{action = <<"pause">>} ->
            wait_for_resume()
    after TheDelay ->
            ok
    end.

wait_for_resume() ->
    receive
        #game_paused{action = <<"resume">>} ->
            ok
    end.

ext_to_board(ExtBoard) ->
    [case Cell of
         null -> null;
         #tavla_checkers{color = C, number = N} -> {fix_color(C), N}
     end || Cell <- ExtBoard].

ext_to_moves(ExtMoves) ->
    [{From, To} || #'TavlaAtomicMoveServer'{from = From, to = To} <- ExtMoves].

get_delay(fast) -> {ok, Val}   = nsm_db:get(config,"games/tavla/robot_delay_fast", 6000), Val;
get_delay(normal) -> {ok, Val} = nsm_db:get(config,"games/tavla/robot_delay_normal", 9000), Val;
get_delay(slow) -> {ok, Val}   = nsm_db:get(config,"games/tavla/robot_delay_slow", 15000), Val.
