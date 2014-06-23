-module(anonymous).
-compile(export_all).
-include_lib("db/include/config.hrl").
-include_lib("kvs/include/user.hrl").

males() ->
  [ "alp","ayberk","doruk","mehmet","ozan","ali","musa","boran","isa",
    "dursun","uzay","taner","halit","yusuf","duman","serdar","halil",
    "emre","kadir","hasan","zeki","ihsan","rıza","kasım","ılgın" ].

females() ->
  [ "betül","eda","lale","pınar","filiz","ezgi","aysu","damla","konca",
    "oylum","ceren","ece","sevil","asena","jale","fatma","arzu","zeynep",
    "dilara","leyla","esra","irem","yasemin","bahar" ].

malesex() -> [ {male,X} || X <- males()].
femalesex() -> [ {female,X} || X <- females()].
names() -> malesex() ++ femalesex().

surnames() -> 
  [ "özçelik","acar","özgür","özkan","tez","vural",
    "akbulut","arslan","avcı","ayhan","baştürk","çağlar","çelik",
    "çetinkaya","demir","dikmen","acar","doğan","ekinci","elmas",
    "erdem","erdoğan","güler","güneş","ilhan","inan","karaca",
    "karadağ","kaya","kemal","keskin","koç","korkmaz","mustafa",
    "osman","özbek","özcan","özdemir","özden","öztürk","paşa",
    "polat","sezer","şahin","sen","şimşek","tekin","tosun","tunç",
    "turan","ünal","yalçın","yazıcı","yıldırım","yılmaz" ].

ima_gio(N) -> {Id,Name,Surname,Sex} = lists:nth(N,imagionary_users()), Id.
ima_gio(N,L) -> {Id,Name,Surname,Sex} = lists:nth(N,L), Id.

imagionary_users() ->
    List = [ begin
        {Sex,X} = SX,
        [HX|TX] = X, NX = [tru(HX)] ++ TX,
        [HY|TY] = Y, NY = [tru(HY)] ++ TY,
        {wf:to_binary([ tr2en(Char) || Char <- unicode:characters_to_list(X++"_"++Y) ]),
         unicode:characters_to_binary(NX),
         unicode:characters_to_binary(NY),Sex}
    end || SX<-names(), Y<-surnames()],
    lists:keysort(1,List).

% Erlang R16

tru(252) -> 220; % $ü -> $Ü
tru(351) -> 350; % $ş -> $Ş
tru(246) -> 214; % $ö -> $Ö
tru(305) -> 304; % $ı -> $İ
tru(287) -> 286; % $ğ -> $Ğ
tru(231) -> 199; % $ç -> $Ç
tru(Ch) -> string:to_upper(Ch).

tr2en(252) -> $u;
tr2en(246) -> $o;
tr2en(231) -> $c;
tr2en(305) -> $i;
tr2en(351) -> $s;
tr2en(252) -> $u;
tr2en(287) -> $g;
tr2en(Sym) -> Sym.

% Erlang R17

%tru($ü) -> $Ü;
%tru($ş) -> $Ş;
%tru($ö) -> $Ö;
%tru($ı) -> $İ;
%tru($ğ) -> $Ğ;
%tru($ç) -> $Ç;
%tru(Ch) -> string:to_upper(Ch).

%tr2en($ü) -> $u;
%tr2en($ö) -> $o;
%tr2en($ç) -> $c;
%tr2en($ı) -> $i;
%tr2en($ş) -> $u;
%tr2en($ğ) -> $g;
%tr2en(Sym) -> Sym.

fake_id() ->
    FakeUsers = imagionary_users(),
    Pos = crypto:rand_uniform(1, length(FakeUsers)),
    H0 = ima_gio(Pos,FakeUsers),
    Id = wf:to_binary(wf:to_list(H0) ++ wf:to_list(id_generator:get_id2())).

fake_id(Login) -> wf:to_binary(wf:to_list(Login) ++ wf:to_list(id_generator:get_id2())).

create_users(A,B) ->
    ImagioUsers = imagionary_users(),
    [ begin 
        {Id,Name,Surname,Sex} = lists:nth(N,ImagioUsers),
        U = #user{  username = Id,
                    id = Id,
                    names = Name,
                    sex = Sex,
                    surnames = Surname,
                    birth={1981,9,29} }, kvs:put(U) end || N <- lists:seq(A, B) ].

virtual_users() ->
    case kvs:get(user,<<"imam@synrc.com">>) of
        {ok,_} -> skip;
        _ -> kvs:join(), create_users(1,100), kvs:put(#user{id= <<"imam@synrc.com">>}) end,

    AllUsers = imagionary_users(),
    F = fun({UserId,_,_,Sex}, Acc) ->
        User = auth_server:get_user_info_by_user_id(UserId),
        case User of
                    {error,_} -> Acc;
                    _ -> [UserId | Acc]
                end
        end,
    lists:usort(lists:foldl(F, [], AllUsers)).

random_users(Num, AllUsers) ->
    AllUsersNum = length(AllUsers),
    random_users(Num, [], AllUsers, AllUsersNum).

random_users(0, Acc, _AllUsers, _AllUsersNum) -> Acc;
random_users(N, Acc, AllUsers, AllUsersNum) ->
    User = lists:nth(crypto:rand_uniform(1, AllUsersNum + 1), AllUsers),
    case lists:member(User, Acc) of
        false -> random_users(N - 1, [User | Acc], AllUsers, AllUsersNum);
        true -> random_users(N, Acc, AllUsers, AllUsersNum)
    end.
