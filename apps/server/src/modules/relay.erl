%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The server for retranslating events of the table.
%%%
%%% Created : Feb 25, 2013
%%% -------------------------------------------------------------------

%% Table -> Relay requests:
%% {register_player, UserId, PlayerId} -> ok
%% {unregister_player, PlayerId, Reason} -> ok

%% Table -> Relay messages:
%% {publish, Event}
%% {to_client, PlayerId, Event}
%% {to_subscriber, SubscrId, Event}
%% {allow_broadcast_for_player, PlayerId}
%% stop

%% Relay -> Table notifications:
%% {player_disconnected, PlayerId}
%% {player_connected, PlayerId}
%% {subscriber_added, PlayerId, SubscriptionId} - it's a hack to retrive current game state

%% Subscriber -> Relay requests:
%% {subscribe, Pid, UserId, RegNum} -> {ok, SubscriptionId} | {error, Reason}
%% {unsubscribe, SubscriptionId} -> ok | {error, Reason}

%% Relay -> Subscribers notifications:
%% {relay_kick, SubscrId, Reason}
%% {relay_event, SubscrId, Event}

-module(relay).

-behaviour(gen_server).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include_lib("server/include/basic_types.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start/1, start_link/1, table_message/2, table_request/2]).
-export([subscribe/4, unsubscribe/2, publish/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {subscribers,
                players,
                observers_allowed  :: boolean(),
                table              :: {atom(), pid()},
                table_mon_ref      :: reference()
               }).

-record(subscriber,
        {id           :: reference(),
         pid          :: pid(),
         user_id      :: binary(),
         player_id    :: integer(),
         mon_ref      :: reference(),
         broadcast_allowed :: boolean()
        }).

-record(player,
        {id           :: integer(),
         user_id      :: binary(),
         status       :: online | offline
        }).

%% ====================================================================
%% External functions
%% ====================================================================


start(Params) ->
    gen_server:start(?MODULE, [Params], []).

start_link(Params) ->
    gen_server:start_link(?MODULE, [Params], []).

subscribe(Relay, Pid, UserId, RegNum) ->
    client_request(Relay, {subscribe, Pid, UserId, RegNum}).

unsubscribe(Relay, SubscriptionId) ->
    client_request(Relay, {unsubscribe, SubscriptionId}).

publish(Relay, Message) ->
    client_message(Relay, {publish, Message}).

table_message(Relay, Message) ->
    gen_server:cast(Relay, {table_message, Message}).

table_request(Relay, Request) ->
    table_request(Relay, Request, 5000).

table_request(Relay, Request, Timeout) ->
    gen_server:call(Relay, {table_request, Request}, Timeout).

client_message(Relay, Message) ->
    gen_server:cast(Relay, {client_message, Message}).

client_request(Relay, Request) ->
    client_request(Relay, Request, 5000).

client_request(Relay, Request, Timeout) ->
    gen_server:call(Relay, {client_request, Request}, Timeout).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
init([Params]) ->
    PlayersInfo = proplists:get_value(players, Params),
    ObserversAllowed = proplists:get_value(observers_allowed, Params),
    Table = {_, TablePid} = proplists:get_value(table, Params),
    Players = init_players(PlayersInfo),
    MonRef = erlang:monitor(process, TablePid),
    {ok, #state{subscribers = subscribers_init(),
                players = Players,
                observers_allowed = ObserversAllowed,
                table = Table,
                table_mon_ref = MonRef}}.

%% --------------------------------------------------------------------
handle_call({client_request, Request}, From, State) ->
    handle_client_request(Request, From, State);

handle_call({table_request, Request}, From, State) ->
    handle_table_request(Request, From, State);

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%% --------------------------------------------------------------------
handle_cast({client_message, Msg}, State) ->
    gas:info(?MODULE,"RELAY_NG Received client message: ~p", [Msg]),
    handle_client_message(Msg, State);

handle_cast({table_message, Msg}, State) ->
    gas:info(?MODULE,"RELAY_NG Received table message: ~p", [Msg]),
    handle_table_message(Msg, State);

handle_cast(_Msg, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
handle_info({'DOWN', TableMonRef, process, _Pid, _Info},
            #state{subscribers = Subscribers,
                   table_mon_ref = TableMonRef} = State) ->
    gas:info(?MODULE,"RELAY_NG All The parent table is down. "
          "Disconnecting all subscribers and sutting down.", []),
    [begin
         erlang:demonitor(MonRef, [flush]),
         Pid ! {relay_kick, SubscrId, table_down}
     end || #subscriber{id = SubscrId, pid = Pid, mon_ref = MonRef}
                           <- subscribers_to_list(Subscribers)],
    {stop, normal, State};

handle_info({'DOWN', MonRef, process, _Pid, _Info},
            #state{subscribers = Subscribers, players = Players,
                   table = {TableMod, TablePid}} = State) ->
    case find_subscribers_by_mon_ref(MonRef, Subscribers) of
        [#subscriber{player_id = undefined, id = SubscrId}] ->
            NewSubscribers = del_subscriber(SubscrId, Subscribers),
            {noreply, State#state{subscribers = NewSubscribers}};
        [#subscriber{player_id = PlayerId, user_id = UserId, id = SubscrId}] ->
            NewSubscribers = del_subscriber(SubscrId, Subscribers),
            case find_subscribers_by_player_id(PlayerId, NewSubscribers) of
                [] ->
                    gas:info(?MODULE,"RELAY_NG All sessions of player <~p> (~p) are closed. "
                          "Sending the notification to the table.", [PlayerId, UserId]),
                    NewPlayers = update_player_status(PlayerId, offline, Players),
                    TableMod:relay_message(TablePid, {player_disconnected, PlayerId}),
                    {noreply, State#state{subscribers = NewSubscribers, players = NewPlayers}};
                _ ->
                    {noreply, State#state{subscribers = NewSubscribers}}
            end;
        [] ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% --------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

handle_client_request({subscribe, Pid, UserId, observer}, _From,
                      #state{observers_allowed = ObserversAllowed,
                             subscribers = Subscribers} = State) ->
    if ObserversAllowed ->
           MonRef = erlang:monitor(process, Pid),
           SubscrId = erlang:make_ref(),
           NewSubscribers = store_subscriber(SubscrId, Pid, UserId, undefined, MonRef, true, Subscribers),
           {reply, ok, State#state{subscribers = NewSubscribers}};
       true ->
           {reply, {error, observers_not_allowed}, State}
    end;


handle_client_request({subscribe, Pid, UserId, PlayerId}, _From,
                      #state{players = Players, subscribers = Subscribers,
                             table = {TableMod, TablePid}} = State) ->
    gas:info(?MODULE,"RELAY_NG Subscription request from user ~p, PlayerId: <~p>", [UserId, PlayerId]),
    case find_player(PlayerId, Players) of
        {ok, #player{user_id = UserId, status = Status} = P} ->  %% The user id is matched
            gas:info(?MODULE,"RELAY_NG User ~p is registered as player <~p>", [UserId, PlayerId]),
            gas:info(?MODULE,"RELAY_NG User ~p player info: ~p", [UserId, P]),
            MonRef = erlang:monitor(process, Pid),
            SubscrId = erlang:make_ref(),
            NewSubscribers = store_subscriber(SubscrId, Pid, UserId, PlayerId, MonRef,
                                              _BroadcastAllowed = false, Subscribers),
            NewPlayers = if Status == offline ->
                                gas:info(?MODULE,"RELAY_NG Notifying the table about user ~p (<~p>).", [PlayerId, UserId]),
                                TableMod:relay_message(TablePid, {player_connected, PlayerId}),
                                update_player_status(PlayerId, online, Players);
                            true ->
                                gas:info(?MODULE,"RELAY_NG User ~p (<~p>) is already subscribed.", [PlayerId, UserId]),
                                Players
                         end,
            TableMod:relay_message(TablePid, {subscriber_added, PlayerId, SubscrId}),
            {reply, {ok, SubscrId}, State#state{players = NewPlayers, subscribers = NewSubscribers}};
        {ok, #player{}=P} ->
            gas:info(?MODULE,"RELAY_NG Subscription for user ~p rejected. There is another owner of the "
                  "PlayerId <~p>: ~p", [UserId, PlayerId, P]),
            {reply, {error, not_player_id_owner}, State};
        error ->
            {reply, {error, unknown_player_id}, State}
    end;


handle_client_request({unsubscribe, SubscrId}, _From,
                      #state{subscribers = Subscribers, players = Players,
                             table = {TableMod, TablePid}} = State) ->
    case get_subscriber(SubscrId, Subscribers) of
        error ->
            {reply, {error, not_subscribed}, State};
        {ok, #subscriber{id = SubscrId, mon_ref = MonRef, player_id = undefined}} ->
            erlang:demonitor(MonRef, [flush]),
            NewSubscribers = del_subscriber(SubscrId, Subscribers),
            {reply, ok, State#state{subscribers = NewSubscribers}};
        {ok, #subscriber{id = SubscrId, mon_ref = MonRef, player_id = PlayerId}} ->
            erlang:demonitor(MonRef, [flush]),
            NewSubscribers = del_subscriber(SubscrId, Subscribers),
            NewPlayers = case find_subscribers_by_player_id(PlayerId, Subscribers) of
                             [] ->
                                TableMod:relay_message(TablePid, {player_disconnected, PlayerId}),
                                update_player_status(PlayerId, offline, Players);
                             _ -> Players
                         end,
            {reply, ok, State#state{subscribers = NewSubscribers,
                                    players = NewPlayers}}
    end;


handle_client_request(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%===================================================================

handle_table_request({register_player, UserId, PlayerId}, _From,
                     #state{players = Players} = State) ->
    NewPlayers = store_player(PlayerId, UserId, offline, Players),
    {reply, ok, State#state{players = NewPlayers}};


handle_table_request({unregister_player, PlayerId, Reason}, _From,
                     #state{players = Players,
                            subscribers = Subscribers} = State) ->
    NewPlayers = del_player(PlayerId, Players),
    UpdSubscribers = find_subscribers_by_player_id(PlayerId, Subscribers),
    F = fun(#subscriber{id = SubscrId, pid = Pid, mon_ref = MonRef}, Acc) ->
                erlang:demonitor(MonRef, [flush]),
                Pid ! {relay_kick, SubscrId, Reason},
                del_subscriber(SubscrId, Acc)
        end,
    NewSubscribers = lists:foldl(F, Subscribers, UpdSubscribers),
    {reply, ok, State#state{players = NewPlayers,
                            subscribers = NewSubscribers}};

handle_table_request(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%===================================================================
%% XXX: Unsecure because of spoofing posibility (a client session can send events by the
%% name of the table). Legacy.
handle_client_message({publish, Msg}, #state{subscribers = Subscribers} = State) ->
    Receipients = subscribers_to_list(Subscribers),
    [Pid ! {relay_event, SubscrId, Msg} ||
     #subscriber{id = SubscrId, pid = Pid, broadcast_allowed = true} <- Receipients],
    {noreply, State};

handle_client_message(_Msg, State) ->
    {noreply, State}.

%%===================================================================

handle_table_message({publish, Msg}, #state{subscribers = Subscribers} = State) ->
    gas:info(?MODULE,"RELAY_NG The table publish message: ~p", [Msg]),
    Receipients = subscribers_to_list(Subscribers),
    [Pid ! {relay_event, SubscrId, Msg} ||
     #subscriber{id = SubscrId, pid = Pid, broadcast_allowed = true} <- Receipients],
    {noreply, State};

handle_table_message({to_client, PlayerId, Msg}, #state{subscribers = Subscribers} = State) ->
    Recepients = find_subscribers_by_player_id(PlayerId, Subscribers),
    gas:info(?MODULE,"RELAY_NG Send table message to player's (~p) sessions: ~p. Message: ~p",
          [PlayerId, Recepients, Msg]),

    %handle_log(PlayerId,Players,Event,State),


    [Pid ! {relay_event, SubscrId, Msg} || #subscriber{id = SubscrId, pid = Pid} <- Recepients],
    {noreply, State};

handle_table_message({to_subscriber, SubscrId, Msg}, #state{subscribers = Subscribers} = State) ->
    gas:info(?MODULE,"RELAY_NG Send table message to subscriber: ~p. Message: ~p", [SubscrId, Msg]),
    case get_subscriber(SubscrId, Subscribers) of
        {ok, #subscriber{pid = Pid}} -> Pid ! {relay_event, SubscrId, Msg};
        _ -> do_nothing
    end,
    {noreply, State};

handle_table_message({allow_broadcast_for_player, PlayerId},
                     #state{subscribers = Subscribers} = State) ->
    gas:info(?MODULE,"RELAY_NG Received directive to allow receiving published messages for player <~p>",
          [PlayerId]),
    PlSubscribers = find_subscribers_by_player_id(PlayerId, Subscribers),
    F = fun(Subscriber, Acc) ->
                store_subscriber_rec(Subscriber#subscriber{broadcast_allowed = true}, Acc)
        end,
    NewSubscribers = lists:foldl(F, Subscribers, PlSubscribers),
    {noreply, State#state{subscribers = NewSubscribers}};


handle_table_message(stop, #state{subscribers = Subscribers} = State) ->
    [begin
         erlang:demonitor(MonRef, [flush]),
         Pid ! {relay_kick, SubscrId, table_closed}
     end || #subscriber{id = SubscrId, pid = Pid, mon_ref = MonRef}
                           <- subscribers_to_list(Subscribers)],
    {stop, normal, State#state{subscribers = subscribers_init()}};


handle_table_message(_Msg, State) ->
    {noreply, State}.

%%===================================================================

init_players(PlayersInfo) ->
    init_players(PlayersInfo, players_init()).

init_players([], Players) ->
    Players;
init_players([{PlayerId, UserId} | PlayersInfo], Players) ->
    NewPlayers = store_player(PlayerId, UserId, offline, Players),
    init_players(PlayersInfo, NewPlayers).

%%===================================================================

players_init() -> midict:new().

store_player(PlayerId, UserId, Status, Players) ->
    store_player_rec(#player{id = PlayerId, user_id = UserId, status = Status}, Players).

store_player_rec(#player{id = PlayerId, user_id = _UserId, status = _Status} = Rec, Players) ->
    midict:store(PlayerId, Rec, [], Players).

del_player(PlayerId, Players) ->
    midict:erase(PlayerId, Players).

fetch_player(PlayerId, Players) ->
    midict:fetch(PlayerId, Players).

find_player(PlayerId, Players) ->
    midict:find(PlayerId, Players).

update_player_status(PlayerId, Status, Players) ->
    Rec = fetch_player(PlayerId, Players),
    store_player_rec(Rec#player{status = Status}, Players).

%%===================================================================

subscribers_init() -> midict:new().

store_subscriber(SubscrId, Pid, UserId, PlayerId, MonRef, BroadcastAllowed, Subscribers) ->
    store_subscriber_rec(#subscriber{id = SubscrId, pid = Pid, user_id = UserId,
                                     player_id = PlayerId, mon_ref = MonRef,
                                     broadcast_allowed = BroadcastAllowed}, Subscribers).

store_subscriber_rec(#subscriber{id = SubscrId, pid = Pid, user_id = _UserId,
                                 player_id = PlayerId, mon_ref = MonRef} = Rec, Subscribers) ->
    midict:store(SubscrId, Rec, [{pid, Pid}, {player_id, PlayerId}, {mon_ref, MonRef}],
                 Subscribers).

%% del_subscriber(SubscrId, Subscribers) -> NewSubscribers
del_subscriber(SubscrId, Subscribers) ->
    midict:erase(SubscrId, Subscribers).

%% get_subscriber(Id, Subscribers) -> {ok, #subscriber{}} | error
get_subscriber(Id, Subscribers) ->
    midict:find(Id, Subscribers).

%% find_subscribers_by_player_id(PlayerId, Subscribers) -> list(#subscriber{})
find_subscribers_by_player_id(PlayerId, Subscribers) ->
    midict:geti(PlayerId, player_id, Subscribers).

%% find_subscribers_by_mon_ref(MonRef, Subscribers) -> list(#subscriber{})
find_subscribers_by_mon_ref(MonRef, Subscribers) ->
    midict:geti(MonRef, mon_ref, Subscribers).

%% subscribers_to_list(Subscribers) -> list(#subscriber{})
subscribers_to_list(Subscribers) ->
    midict:all_values(Subscribers).
