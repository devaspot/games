-module(okey).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").
-include_lib("avz/include/avz.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("db/include/journal.hrl").

-define(GAMEID, game_form()).

game_form() ->
    case wf:qs(<<"game">>) of
        undefined -> 1000001;
        X -> wf:to_integer(X) end.

new_user() ->
    Imagionary = anonymous:imagionary_users(),
    {Id,Name,Surname,Sex} = lists:nth(crypto:rand_uniform(1,length(Imagionary)),Imagionary),
    FakeId = anonymous:fake_id(Id),
    X = #user{
        id = FakeId,
        sex = Sex,
        tokens=[{n2o,get(session_id)}],
        names = Name,
        surnames = Surname},
    wf:wire(wf:f("document.cookie='~s=~s; path=/; expires=~s';",
        ["n2o-name",wf:to_list(FakeId),js_session:cookie_expire(js_session:ttl())])),
    kvs:put(X),
    X.

new_facebook_user(User) ->
    ExistingUser = user(),
    Score = proplists:get_value(score,ExistingUser#user.tokens,0),
    Skill = case kvs:get(reveal_log, User#user.id) of
       {ok,RL} -> RL#reveal_log.skill;
       _ -> 0 end,
    U1 = User#user{tokens=game:plist_setkey(score,1,User#user.tokens,{score,Score})},
    U2 = U1#user{tokens=game:plist_setkey(n2o,1,U1#user.tokens,{n2o,get(session_id)})},
    U3 = U2#user{tokens=game:plist_setkey(skill,1,U2#user.tokens,{skill,Skill})},
    kvs:put(U3),
    send_auth_cookies(U3),
    U3.

send_auth_cookies(User) ->
    wf:wire(wf:f("document.cookie='~s=~s; path=/; expires=~s';",
        ["n2o-name",wf:to_list(User#user.id),
        js_session:cookie_expire(js_session:ttl())])),
    wf:wire(wf:f("document.cookie='~s=~s; path=/; expires=~s';",
        ["n2o-sid",wf:to_list(proplists:get_value(n2o,User#user.tokens)),
        js_session:cookie_expire(js_session:ttl())])),
    wf:user(User),
    ok.

user() -> 
    case wf:user() of
        undefined ->
            SessionUser = wf:cookie_req(<<"n2o-name">>,?REQ),
            SessionId = get(session_id),
            wf:info(?MODULE,"Auth User: ~p",[SessionUser]),
            wf:info(?MODULE,"Auth Id: ~p",[SessionId]),
            X = case kvs:get(user,SessionUser) of
                {ok,User} ->
                    SS = lists:keyfind(n2o,1,User#user.tokens),
                    case SS of
                        {n2o,SessionId} -> User;
                        _ -> new_user() end;
                _ -> new_user() end,
            wf:user(X),
            X;
        U-> U end.

player_name(PI) -> auth_server:player_name(PI).

main() -> []. %#dtl{file="index", bindings=[{title,<<"N2O">>},{body,[]}]}.

patch_users() ->
    [ begin
        Score = score_journal(User),
        Sex = case User#user.sex of undefined -> male; S -> S end,
        case Score of
           0 -> kvs:delete(user,User#user.id);
           _ -> kvs:put(User#user{sex=Sex,
                   tokens=game:plist_setkey(score,1,Tokens,{score,Score})}) end
    end|| User=#user{tokens=Tokens} <- kvs:all(user), Tokens /= [], Tokens /= undefined].

send_roster(Pid) ->
%    X = [ send_roster_item(User) || User=#user{tokens=Tokens} <- kvs:all(user), Tokens /= [], Tokens /= undefined],
    X = [ begin
       {User#user.id,User#user.names,User#user.surnames,integer_to_binary(score(User))}
       end || User=#user{tokens=Tokens} <- kvs:all(user), Tokens /= [], Tokens /= undefined, proplists:get_value(score,Tokens,0) /= 0],
    XS = lists:sort(fun({_,_,_,S1},{_,_,_,S2}) -> 
         binary_to_integer(S1) > binary_to_integer(S2) end,X),
    Lists = [lists:sublist(XS,100)], %split(170,XS,[]),
    [ send_roster_group(Pid,List) || List <- Lists],
    Pid ! {server,{roster_end,length(Lists)}},
    wf:info(?MODULE,"Users: ~p",[length(X)]).

split(N,[],Result) -> Result; 
split(N,List,Result) when length(List) < N -> Result ++ [List];
split(N,List,Result) -> {A,B}=lists:split(N,List), Result ++ [A] ++ split(N,B,Result). 

score(User) -> proplists:get_value(score,User#user.tokens,0).

score_journal(User) ->
    Score = case kvs:get(reveal_log,User#user.id) of
        {ok,#reveal_log{score=S}} -> S;
        _ -> wf:info(?MODULE,"Score not found for User ~p",[User#user.id]), 0 end.

already_online(Pid) ->
    [ Pid ! {user_online,User} || {_,_,{_,User}} <- game:online() ].

send_roster_item(Pid,User) ->
    Pid ! {server,{roster_item,User#user.id,User#user.names,User#user.surnames,0}}.

send_roster_group(Pid,List) ->
    wf:info(?MODULE,"User Group: ~p",[List]),
    Pid ! {server,{roster_group,List}}.

event(terminate) -> 
    User = user(),
    wf:send(broadcast,{user_offline,User}),
    wf:info(?MODULE,"EXTerminate",[]);

event(init) -> 
    js_session:ensure_sid([],?CTX),

    event(attach),
    event(join);

event(join) -> 
    GameId = get(okey_game_id),
    wf:wire(#api{name=fbLogin, tag=fb}),
    wf:wire(protocol:join(wf:to_list(GameId)));

event(attach) -> 
    {ok,GamePid} = game_session:start_link(self()),
    wf:session(<<"game_pid">>,GamePid),
    User = user(),
    wf:reg(User#user.id),
    wf:info(?MODULE,"User Attach: ~p",[User]),
    gproc:set_value({p,l,broadcast},{wf:peer(?REQ),User}),
    wf:info(?MODULE,"Games Online: ~p",[game:online()]),
    put(okey_im, User#user.id),
    wf:wire(wf:f("document.user = '~s';document.names = '~s';document.surnames = '~s';",
                        [User#user.id,User#user.names,User#user.surnames])),
    wf:info(?MODULE,"Session User: ~p",[User]),
    GameId = case wf:q(games_ids) of undefined -> ?GAMEID; Res -> Res end,
    put(okey_game_id, GameId),
    Token = auth_server:generate_token(GameId,User#user.id),
    wf:info(?MODULE,"Game Token: ~p",[Token]),
    wf:wire(protocol:attach(wf:f("'~s'",[Token]))),
    Pid = self(),
    spawn(fun() ->
        send_roster(Pid),
        already_online(Pid),
        wf:send(broadcast,{user_online,User})
    end),
    ok;

event(logout) ->
    wf:wire(protocol:logout());

event({client,{message,From,Name,To,Message}}) ->
    wf:info(?MODULE,"Online Chat Message from ~p(~p) to ~p:~n ~p~n",[From,Name,To,Message]),
    wf:send(To,{server,{chat_message,{From,Name},To,wf:to_binary(Message)}}),
    ok;

event({client,Message}) ->
    wf:info(?MODULE,"Client: ~p", [Message]),
    case wf:session(<<"game_pid">>) of
        undefined -> skip;
        GamePid -> SyncRes = game_session:process_request(GamePid, Message),
                   wf:info(?MODULE,"Sync Result: ~p",[SyncRes]) end;

event({server,{roster_group,List}}) -> skip;
event({server,terminate}) -> event(terminate);
event({server,{update_score,Score}}) -> 
    User = user(),
    NewUser = User#user{tokens=game:plist_setkey(score,1,User#user.tokens,{score,Score})},
    wf:user(NewUser),
    gproc:set_value({p,l,broadcast},{wf:peer(?REQ),NewUser}),
    wf:info(?MODULE,"User Process Updated Score ~p ~p",[User#user.id,Score]),
    wf:send(broadcast,{user_online,NewUser}),
    ok;
event({counter,Res}) -> Pid = self(), spawn(fun() -> Pid ! {server,{online_number,length(game:online())}} end);
event({user_online,User}) ->
    wf:info(?MODULE,"User ~p goes Online",[User#user.id]),
    Id = User#user.id,
    Names = User#user.names,
    Surnames = User#user.surnames,
    Score = score(User),
    self() ! {server,{online,Id,Names,Surnames,integer_to_binary(Score)}};

event({user_offline,User}) ->
    Id = User#user.id,
    Names = User#user.names,
    Surnames = User#user.surnames,
    Score = score(User),
    self() ! {server,{offline,Id,Names,Surnames,integer_to_binary(Score)}};

event({register,User}) ->
    wf:info(?MODULE,"Register: ~p",[User]),
    wf:send(broadcast,{user_offline,user()}),
    new_facebook_user(User),
    wf:wire("window.location='https://kakaranet.com'");

event({login,User}) ->
    wf:info(?MODULE,"Login: ~p",[User]),
    wf:send(broadcast,{user_offline,user()}),
    send_auth_cookies(User),
%    event(logout),
%    event(attach),
%    event(join),
    wf:wire("window.location='https://kakaranet.com'");

event(_Event) -> wf:info(?MODULE,"Unknown Event: ~p", [_Event]).


api_event(X,Y,Z) -> avz:api_event(X,Y,Z).
