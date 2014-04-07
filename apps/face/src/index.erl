-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").

main() -> 
    case wf:user() of
         undefined -> wf:redirect("/login"), #dtl{file="index",app=n2o_sample,bindings=[{title,""},{body,""}]};
         _ -> #dtl{file = "index", app=n2o_sample,bindings=[{title,title()},{body,body()}]}
     end.

title() -> [ <<"N2O">> ].

body() ->
    {ok,Pid} = wf:comet(fun() -> chat_loop() end), 
    [ #span{ body = io_lib:format("'/index?x=' is ~p",[wf:qs(<<"x">>)]) },
      #panel{ id=history },
      %%#textbox{ id=message },
      %%#button{ id=send, body= <<"Chat">>, postback={chat,Pid}, source=[message] },
      %%#button{ id=test, body= <<"Test">>, postback={test,Pid}},
      %% session attachm, join game, take %1, discard %1
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #button{ id = take, body = <<"Take">>, postback = take},
      #button{ id = discard, body = <<"Discard">>, postback = discard}
    ].

event(init) ->
    {ok,GamePid} = game_session:start_link(self()),
    put(game_session,GamePid),
    User = wf:user(),
    wf:reg(room),
    X = wf:qs(<<"x">>),
    wf:insert_bottom(history, [ #span{id=text, body = io_lib:format("User ~s logged in. X = ~p", [User,X]) },
                                #button{id=logout, body="Logout", postback=logout}, #br{} ]);

event({chat,Pid}) ->
    GamePid = get(game_session),
%%    game_session:process_request(GamePid,"Web Site",#session_attach{token=?TEST_TOKEN}), 
%%    game_session:process_request(GamePid,"Web Site",#join_game{game=1000001}),
    error_logger:info_msg("Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    Message = wf:q(message),
    wf:wire(#jq{target=message,method=[focus,select]}),
    wf:update(text,[#panel{body= <<"Text">>},#panel{body= <<"OK">>}]),
    Pid ! {message, Username, Message};

%%event({Event}) ->
%%    wf:wire("ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.atom('ok2') )));");

event(attach) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('session_attach'), '" ++ ?TEST_TOKEN ++ "'))));",
    wf:wire(Msg),
    wf:update(text,[#panel{body= <<"Text">>},#panel{body= <<"OK">>}]);

event(join) ->
    Msg = "ws.send(Bert.encodebuf(Bert.tuple(Bert.atom('client'), Bert.tuple(Bert.atom('join_game'), 1000001))));",
    wf:wire(Msg),
    wf:update(text,[#panel{body= <<"Text">>},#panel{body= <<"OK">>}]);

%%-record(okey_take, {
%%          pile :: integer() %% 0 or 1
%%         }).

event(take) ->
    Message = "Bert.tuple()",
    wf:update(text,[#panel{body= <<"Text">>},#panel{body= <<"OK">>}]);

event(logout) -> 
    wf:logout(),
    <<"/ws/",X,_/binary>> = wf:path(?REQ),
    case X of
        $i -> wf:redirect("/login");
        $l -> wf:redirect("/login");
         _ -> wf:redirect("/static/spa/spa.htm") end;
event(login) -> login:event(login);
event(continue) -> wf:info("OK Pressed");

event(Event) -> wf:info("Event: ~p", [Event]).

chat_loop() ->
    receive 
        {message, Username, Message} ->
            Terms = [ #span { body=Username }, [": "], #span { body=Message }, #br{} ],
            wf:insert_bottom(history, Terms),
            wf:wire("$('#chatHistory').scrollTop = $('#chatHistory').scrollHeight;"),
            wf:flush(room);
        Unknown -> error_logger:info_msg("Unknown Looper Message ~p",[Unknown])
    end,
    chat_loop().
