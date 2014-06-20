-module(journal).
-behaviour(gen_server).
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("db/include/journal.hrl").
-include_lib("server/include/game_state.hrl").
-include_lib("server/include/requests.hrl").
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {history = []}).

start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
mypid() -> gen_server:call(?SERVER, mypid).
clear_history() -> gen_server:cast(?SERVER, clear_history).
get_history() -> gen_server:call(?SERVER, get_history).
exit() -> gen_server:call(?SERVER, terminate).
protocol_event(User,Event,State) -> gen_server:cast(?SERVER, {protocol_event, User, Event, State}).
series_event(User,Event,State) -> gen_server:cast(?SERVER, {series_event, User, Event, State}).
reveal_event(User,Event,State) -> gen_server:cast(?SERVER, {reveal_event, User, Event, State}).
update_stats(User,Event,Pos,State) -> gen_server:cast(?SERVER, {update_stats, User, Event, Pos, State}).
timestamp() -> {MegaSec, Sec, MiliSec} = erlang:now(), MegaSec * 1000 * 1000 * 1000  + Sec * 1000 + MiliSec.

init([]) ->
    wf:reg(stats),
    {ok, #state{}}.
handle_call(mypid, _From, State) -> {reply, {ok, self()}, State};
handle_call(get_history, _From, #state{history = History} = State) -> {reply, {ok, lists:reverse(History)}, State};
handle_call(_Request, _From, State) -> 
    gas:info(?MODULE, "Event Log: call message ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.
handle_cast({protocol_event, UserId,
    #game_event{event=EventName}=Event, GameState}, State) ->

    {Date,Time} = calendar:local_time(),
    GameKind = element(#table_state.tournament_type, GameState),
    GameMode = element(#table_state.game_mode, GameState),
    GameId = element(#table_state.game_id, GameState),
    Speed = element(#table_state.speed, GameState),
    Rounds = element(#table_state.rounds, GameState),

    Key = {GameKind,GameMode,Speed,Rounds,GameId},

    ProtocolEvent = 
        #protocol_event{
           user = UserId,
           feed_id = Key,
           id = timestamp(),
           game_id = GameId,
           date = Date,
           time = Time,
           module = GameKind,
           speed = Speed,
           rounds = Rounds,
           event = EventName,
           game_event = Event},

%    gas:info(?MODULE, "ProtocolEvent: ~p", [ProtocolEvent]),

    try kvs:add(ProtocolEvent)
    catch E:R ->
        gas:info(?MODULE,"kvs:add ERROR ~p",[{E,R}]),
        gas:info(?MODULE, "Errored Protocol Event: ~p", [ProtocolEvent]),
        ok end,

    update_container_stats(UserId, ProtocolEvent,#protocol_event.event,GameState),

    {noreply, State};
handle_cast({update_stats, User, Event, Pos, GameState}, State) ->
    update_container_stats(User, Event, Pos, GameState),
    {noreply, State};
handle_cast({reveal_event, User, Event, GameState}, State) ->
    kvs:add(Event),
    update_container_stats(User, Event, #reveal_event.reason, GameState),
    SE = case kvs:get(reveal_log,Event#reveal_event.feed_id) of
  	  {error,_} -> #reveal_log{};
  	  {ok, RL} -> RL end,
    Skill = case SE#reveal_log.skill of X when is_integer(X) -> X; _ -> 0 end,
    LogScore = case SE#reveal_log.score of X1 when is_integer(X1) -> X1; _ -> 0 end,
    Score = Event#reveal_event.score,
    NewScore = case kvs:get(user,User) of
        {ok,U=#user{tokens=Tokens}} ->
            ScoreFromUser = Score+proplists:get_value(score,U#user.tokens,0),
            kvs:put(U#user{tokens=game:plist_setkey(score,1,Tokens,{score,ScoreFromUser})}),
            wf:send(User,{server,{update_score,ScoreFromUser}}),
            ScoreFromUser;
        _ ->
            ScoreFromLog = Score+LogScore,
            wf:send(User,{server,{update_score,ScoreFromLog}}),
            ScoreFromLog
    end,
    gas:info(?MODULE, "Reveal Upadtes User Record: ~p ~p", [User,NewScore]),
    kvs:put(SE#reveal_log{skill=Skill+1,score=NewScore}),
    {noreply, State};
handle_cast({series_event, User, Event, GameState}, State) ->
    kvs:add(Event),
    update_container_stats(User, Event, #series_event.result, GameState),
    {ok, SE} = kvs:get(series_log,Event#series_event.feed_id),
    Score = case SE#series_log.score of X when is_integer(X) -> X; _ -> 0 end,
    kvs:put(SE#series_log{score=Score+Event#series_event.score}),
    {noreply, State};
handle_cast(clear_history, State) -> {noreply, State#state{history = []}};
handle_cast(_Msg, State) -> gas:info(?MODULE, "Event Log: cast message ~p", [_Msg]), {noreply, State}.
handle_info({stats,Route,Message}, State) ->
    gas:info(?MODULE,"Stats: Route: ~p Message: ~p~n",[Route,Message]),
    handle_stats(Route,Message),
    {noreply, State};
handle_info(_Info, State) -> gas:info(?MODULE, "Event Log: info message ~p", [_Info]), {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_stats([tournament,T,cancel],Message) -> ok;
handle_stats([tournament,T,activate],Message) -> ok;
handle_stats([personal_score,user,U,add],Message) -> ok;
handle_stats([system,game_end_note,U,add],Message) -> ok;
handle_stats([system,tournament_tour_note,T],Message) -> ok;
handle_stats([system,tournament_ends_note,T],Message) -> ok;
handle_stats([system,game_ends_note,T],Message) -> ok;
handle_stats(Route,Message) -> ok.

update_container_stats(User,Event,Pos,GameState) ->
    {Date,Time} = calendar:local_time(),

    GameKind = element(#table_state.tournament_type,GameState),
    GameMode = element(#table_state.game_mode,GameState),
    GameId   = element(#table_state.game_id,GameState),
    Speed    = element(#table_state.speed,GameState),
    Rounds   = element(#table_state.rounds,GameState),

    ContainerName = element(#container_event.container,Event),
    FeedId = element(#container_event.feed_id,Event),
    NC = case kvs:get(ContainerName,FeedId) of
        {ok,GL} -> GL;
        _ -> list_to_tuple([ContainerName|proplists:get_value(ContainerName, kvs:containers())]) end,

    C1 = setelement(#container_log.id,     NC, FeedId),
    C2 = setelement(#container_log.speed,  C1, Speed),
    C3 = setelement(#container_log.rounds, C2, Rounds),
    C4 = setelement(#container_log.type,   C3, GameMode),
    C5 = setelement(#container_log.module, C4, GameKind),
    C6 = setelement(#container_log.date,   C5, Date),
    C7 = setelement(#container_log.time,   C6, Time),
    C8 = setelement(#container_log.user,   C7, User),
    C9 = setelement(#container_log.game_id,C8, GameId),

    Container = C9,

    ContainerStats = element(#container_log.stats, Container),
    EventName = element(Pos,Event),
    PS = case is_list(ContainerStats) of true -> ContainerStats; _ -> [] end,
    Stats = case lists:keyfind(EventName,1,PS) of
        {EventName,Count} -> lists:keyreplace(EventName,1,PS,{EventName,Count+1});
        false -> [{EventName,1}|PS] end,
    NewContainer = setelement(#container_log.stats,Container,Stats),
    kvs:put(NewContainer).
