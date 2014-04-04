-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").

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
      #textbox{ id=message },
      #button{ id=send, body= <<"Chat">>, postback={chat,Pid}, source=[message] } ].

event(init) ->
    User = wf:user(),
    wf:reg(room),
    X = wf:qs(<<"x">>),
    wf:insert_bottom(history, [ #span{id=text, body = io_lib:format("User ~s logged in. X = ~p", [User,X]) },
                                #button{id=logout, body="Logout", postback=logout}, #br{} ]);

event({chat,Pid}) ->
    error_logger:info_msg("Chat Pid: ~p",[Pid]),
    Username = wf:user(),
    Message = wf:q(message),
    wf:wire(#jq{target=message,method=[focus,select]}),
    wf:update(text,[#panel{body= <<"Text">>},#panel{body= <<"OK">>}]),
    Pid ! {message, Username, Message};

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
