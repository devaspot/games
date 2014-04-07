-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login"), #dtl{file="index",app=n2o_sample,bindings=[{title,""},{body,""}]};
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{title,<<"N2O">>},{body,body()}]}
     end.

body() ->
    [ #panel{ id=history },
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #button{ id = take, body = <<"Take">>, postback = take},
      #button{ id = discard, body = <<"Discard">>, postback = discard}
    ].

event(init) ->
    {ok,GamePid} = game_session:start_link(self()),
    put(game_session,GamePid);

event(attach) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('session_attach'), '" ++ ?TEST_TOKEN ++ "'))));",
    wf:wire(Msg);

event(join) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('join_game'), 1000001))));",
    wf:wire(Msg);


%%-record(game_action, {
%%          game      :: 'GameId'(),
%%          action    :: string(),
%%          args = [] :: proplist()
%%         }).

%%-record(okey_take, {
%%          pile :: integer() %% 0 or 1
%%         }).

event(take) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('game_action'), 1000001, 'okey_take', [Bert.tuple(Bert.atom('pile'), 0))))));",
    wf:wire(Msg);

%%-record('OkeyPiece', {
%%          color = -1 :: integer(),           %% 1..4
%%          value = -1 :: integer()            %% 1..13
%%          %% color set to 1 and value set to zero mean that this is false okey
%%         }).


%%-record(okey_discard, {
%%          tile :: #'OkeyPiece'{}
%%         }).




event(Event) -> wf:info("Event: ~p", [Event]).

