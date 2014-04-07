-module(index).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").
-jsmacro([take/2]).

take(GameId,Place) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(
            bert:atom("game_action"),
            GameId,
            bert:atom("okey_take"),
            [{pile,Place}])))).

main() -> #dtl{file="index", bindings=[{title,<<"N2O">>},{body,body()}]}.

body() ->
    [ #panel{ id=history },
      #dropdown { id=drop, value="2", postback=combo, source=[drop], options=[
        #option { label= <<"Option 1">>, value= <<"1">> },
        #option { label= <<"Option 2">>, value= <<"2">> },
        #option { label= <<"Option 3">>, value= <<"3">> }
     ]},
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #button{ id = take, body = <<"Take">>, postback = take},
      #button{ id = discard, body = <<"Discard">>, postback = discard}
    ].

event(init) ->
    {ok,GamePid} = game_session:start_link(self()),
    put(game_session,GamePid);

event(combo) ->
    wf:info("Combo: ~p",[wf:q(drop)]);

event(attach) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('session_attach'), '" ++ ?TEST_TOKEN ++ "'))));",
    wf:wire(Msg);

event(join) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('join_game'), 1000001))));",
    wf:wire(Msg);

event(take) -> wf:wire(take("1000001","0"));

event(Event) -> wf:info("Event: ~p", [Event]).

