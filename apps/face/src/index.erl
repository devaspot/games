-module(index).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").
-jsmacro([take/2]).

take(GameId,Place) ->
    ws:send(bert:encodebuf(bert:tuple(bert:atom('client'),
        bert:tuple(bert:atom("game_action"),GameId,bert:atom("okey_take"),[{pile,Place}])))).

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login"), #dtl{file="index",app=n2o_sample,bindings=[{title,""},{body,""}]};
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{title,<<"N2O">>},{body,body()}]}
     end.

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
      #dropdown { id=dropdown1, value="2", options=[
                                                    #option { body="Option 1", value="1" },
                                                    #option { body="Option 2", value="2" },
                                                    #option { body="Option 3", value="3" }
                                                   ]},
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


%%-record(game_action, {
%%          game      :: 'GameId'(),
%%          action    :: string(),
%%          args = [] :: proplist()
%%         }).

%%-record(okey_take, {
%%          pile :: integer() %% 0 or 1
%%         }).



event(take) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'),"
        " Bert.tuple(Bert.atom('game_action'), 1000001, Bert.atom('okey_take'), {pile: 0} ) )));",
    wf:wire(take("1000001","0"));

%%-record('OkeyPiece', {
%%          color = -1 :: integer(),           %% 1..4
%%          value = -1 :: integer()            %% 1..13
%%          %% color set to 1 and value set to zero mean that this is false okey
%%         }).


%%-record(okey_discard, {
%%          tile :: #'OkeyPiece'{}
%%         }).


event(discard) ->
    Color = "1", %%wf:q(cardcolor),
    Value = "1", %%wf:q(cardvalue),
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'),"
        " Bert.tuple(Bert.atom('game_action'), 1000001, Bert.atom('okey_discard')," 
        " [Bert.tuple(Bert.atom('tile'), Bert.tuple(Bert.atom('OkeyPiece'), " ++ Color ++ ", " ++ Value ++ "))]) )));",
    wf:wire(Msg);

event(Event) -> wf:info("Event: ~p", [Event]).

