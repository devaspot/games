-module(game_app).
-behaviour(application).
-export([start/2, start/0, stop/0, stop/1]).
-include_lib("eunit/include/eunit.hrl").

start() -> start(init,[]).
start(_StartType, _StartArgs) -> game_sup:start_link().
stop() -> stop([]).
stop(_State) -> game_sup:stop().
