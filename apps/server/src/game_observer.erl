%%%-------------------------------------------------------------------
%%% @author Dayneko Roman <me@h0.org.ua>
%%% @copyright (C) 2014, Dayneko Roman
%%% @doc
%%%
%%% @end
%%% Created : 22 Apr 2014 by Dayneko Roman <me@h0.org.ua>
%%%-------------------------------------------------------------------
-module(game_observer).

-behaviour(gen_server).

-include_lib("kvs/include/kvs.hrl").
-include_lib("db/include/game_event_container.hrl").
-include_lib("server/include/requests.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% api
-export([mypid/0, clear_history/0, get_history/0, log_event/1]).

-define(SERVER, ?MODULE).

-record(state, {history = []}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

mypid() ->
    gen_server:call(?SERVER, mypid).

clear_history() ->
    gen_server:cast(?SERVER, clear_history).

get_history() ->
    gen_server:call(?SERVER, get_history).

log_event(Event) ->
    gen_server:cast(?SERVER, {log_event, Event}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(mypid, _From, State) ->
    {reply, {ok, self()}, State};
handle_call(get_history, _From, #state{history = History} = State) ->
    {reply, {ok, lists:reverse(History)}, State};
handle_call(_Request, _From, State) ->
    gas:info(?MODULE, ">>>>>>>>>>>>>>>>> call message ~p from ~p", [_Request, _From]),
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({log_event, #game_event{game = GameId, event = EventName, args = Args} = Event}, #state{history = History} = State) ->

    PlayerId = case lists:keyfind(player, 1, Args) of {_, Id} -> Id; _ -> <<"unknow">> end,
    
    Container = 
        #game_event_container{
           feed_id = {GameId, PlayerId},
           id = {timestamp(), GameId, PlayerId},
           game_id = GameId,
           event = EventName,
           timestamp = calendar:now_to_universal_time(erlang:now()), %% date in universal time
           game_event = Event},
 
    gas:info(?MODULE, ">>>>>>>>>>>>>>>>> Container ~p", [Container]),
    
    kvs:add(Container), %% will be spamming kvs

    {noreply, State#state{history = [Event | History]}};
handle_cast(clear_history, State) ->
    {noreply, State#state{history = []}};
handle_cast(_Msg, State) ->
    gas:info(?MODULE, ">>>>>>>>>>>>>>>>> cast message ~p", [_Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%%handle_info({relay_event, _Ref, #game_event{game = GameId, event = Event, args = Args} = GameEvent},
%%            #state{history = History} = State) ->
%%    
%%    {_, PlayerId} = lists:keyfind(player, 1, Args),
%%    
%%    Container = 
%%        #game_event_container{
%%           feed_id = {GameId, PlayerId},
%%           id = {timestamp(), GameId, PlayerId},
%%           game_id = GameId,
%%           event = Event,
%%           timestamp = calendar:now_to_universal_time(erlang:now()), %% date in universal time
%%           game_event = GameEvent},
%% 
%%    gas:info(?MODULE, ">>>>>>>>>>>>>>>>> Container ~p", [Container]),
%%    
%%    kvs:add(Container), %% will be spamming kvs
%%    {noreply, State#state{history = [Event | History]}};
handle_info(_Info, State) ->
    gas:info(?MODULE, ">>>>>>>>>>>>>>>>> info message ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


timestamp() ->
    {MegaSec, Sec, MiliSec} = erlang:now(),
    MegaSec * 1000 * 1000 * 1000  + Sec * 1000 + MiliSec.
