-module(id_generator).
-behaviour(gen_server).
-export([start_link/0]).
-export([get_id/0,get_id2/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-define(SERVER, ?MODULE).
-record(state, {lastid = 1000000, robotid = 1500000}).

get_id() -> gen_server:call(?MODULE, get_id).
get_id2() -> gen_server:call(?MODULE, get_id2).
start_link() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> {ok, #state{lastid=wf:config(nsx_idgen,game_pool,1000000)}}.
handle_call(get_id, _From, #state{lastid = LID} = State) -> Reply = LID + 1, {reply, Reply, State#state{lastid = Reply}};
handle_call(get_id2, _From, #state{robotid = RID} = State) -> Reply = RID + 1, {reply, Reply, State#state{robotid = Reply}};
handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
