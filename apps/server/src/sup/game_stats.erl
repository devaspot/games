-module(game_stats).
-behaviour(gen_server).
-compile(export_all).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("server/include/basic_types.hrl").
-include_lib("server/include/game_okey.hrl").
-include_lib("server/include/game_tavla.hrl").
-include_lib("db/include/transaction.hrl").
-include_lib("db/include/scoring.hrl").

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    wf:reg(stats),
    {ok, no_state}.

handle_call(Request, From, State) ->
    error_logger:error_msg("unknown call ~p ~p ~n", [Request, From]),
    {noreply, State}.

handle_cast(Msg, State) -> error_logger:error_msg("unknown cast ~p ~n", [Msg]), {noreply, State}.
handle_info({stats,Route,Message}, State) ->
    wf:info("Stats: Route: ~p Message: ~p~n",[Route,Message]),
    handle_stats(Route,Message),
    {noreply, State};

handle_info(Info, State) -> error_logger:error_msg("unknown info ~p~n", [Info]), {noreply, State}.

handle_stats([tournament,T,cancel],Message) -> ok;
handle_stats([tournament,T,activate],Message) -> ok;
handle_stats([personal_score,user,U,add],Message) -> ok;
handle_stats([system,game_end_note,U,add],Message) -> ok;
handle_stats([system,tournament_tour_note,T],Message) -> ok;
handle_stats([system,tournament_ends_note,T],Message) -> ok;
handle_stats([system,game_ends_note,T],Message) -> ok;
handle_stats(Route,Message) -> ok.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

