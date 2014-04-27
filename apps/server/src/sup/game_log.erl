-module(game_log).
-behaviour(gen_server).
-include_lib("kvs/include/kvs.hrl").
-include_lib("db/include/game_log.hrl").
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
protocol_event(User,Event,State) -> gen_server:cast(?SERVER, {protocol_event, User, Event, State}).
reveal_event(User,Event,State) -> gen_server:cast(?SERVER, {reveal_event, User, Event, State}).
update_stats(User,Event,Pos,State) -> gen_server:cast(?SERVER, {update_stats, User, Event, Pos, State}).
timestamp() -> {MegaSec, Sec, MiliSec} = erlang:now(), MegaSec * 1000 * 1000 * 1000  + Sec * 1000 + MiliSec.

init([]) -> {ok, #state{}}.
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

    EventLogEntry = 
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

    gas:info(?MODULE, "Event Log: ~p", [EventLogEntry]),
    kvs:add(EventLogEntry),
    update_container_stats(UserId, EventLogEntry,#protocol_event.event,GameState),

    {noreply, State};
handle_cast({update_stats, User, Event, Pos, GameState}, State) ->
    update_container_stats(User, Event, Pos, GameState),
    {noreply, State};
handle_cast({reveal_event, User, Event, GameState}, State) ->
    kvs:add(Event),
    update_container_stats(User, Event, #reveal_event.reason, GameState),
    {noreply, State};
handle_cast(clear_history, State) -> {noreply, State#state{history = []}};
handle_cast(_Msg, State) -> gas:info(?MODULE, "Event Log: cast message ~p", [_Msg]), {noreply, State}.
handle_info(_Info, State) -> gas:info(?MODULE, "Event Log: info message ~p", [_Info]), {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

update_container_stats(User,Event,Pos,GameState) ->
    {Date,Time} = calendar:local_time(),

    GameKind = element(#table_state.tournament_type,GameState),
    GameMode = element(#table_state.game_mode,GameState),
    GameId = element(#table_state.game_id,GameState),
    Speed = element(#table_state.speed,GameState),
    Rounds = element(#table_state.rounds,GameState),

    ContainerName = element(#container_event.container,Event),
    FeedId = element(#container_event.feed_id,Event),
    Container = case kvs:get(ContainerName,FeedId) of
        {ok,GL} -> GL;
        _ ->
            NC = list_to_tuple([ContainerName|proplists:get_value(ContainerName, kvs:containers())]),
            C1 = setelement(#container_log.id,     NC, FeedId),
            C2 = setelement(#container_log.speed,  C1, Speed),
            C3 = setelement(#container_log.rounds, C2, Rounds),
            C4 = setelement(#container_log.type,   C3, GameMode),
            C5 = setelement(#container_log.module, C4, GameKind),
            C6 = setelement(#container_log.date,   C5, Date),
            C7 = setelement(#container_log.time,   C6, Time),
            C8 = setelement(#container_log.user,   C7, User),
            C8

            end,
    ContainerStats = element(#container_log.stats, Container),
    EventName = element(Pos,Event),
    PS = case is_list(ContainerStats) of true -> ContainerStats; _ -> [] end,
    Stats = case lists:keyfind(EventName,1,PS) of
        {EventName,Count} -> lists:keyreplace(EventName,1,PS,{EventName,Count+1});
        false -> [{EventName,1}|PS] end,
    NewContainer = setelement(#container_log.stats,Container,Stats),
    kvs:put(NewContainer).
