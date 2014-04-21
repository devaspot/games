-module (routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?ROUTING_API).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#context.req),
	wf:info("ROute: ~p",[Path]),
    
    Module = route_prefix(Path),
    {ok, State, Ctx#context{path=Path,module=Module}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>)              -> okey;
route(<<"index">>)       -> okey;
route(<<"favicon.ico">>) -> static_file;
route(_) -> okey.
