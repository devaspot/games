-module(anonymous).
-compile(export_all).
-include_lib("db/include/config.hrl").
-include_lib("kvs/include/user.hrl").

names() ->
   ["pinar","betul","eda","lale","ilgin","alp","ayberk","mehmet","ozan","doruk",
    "duman","boran","dursun","taner","uzay","ali","musa","halit","yusuf","isa",
    "asena","aysu","konca","ceren","oylum","filiz","ezgi","ece","sevil","damla",
    "bahar","arzu","dilara","esra","leyla","jale","fatma","irem","yasmin","zeynep",
    "magnolia","jenifer","roksolana","tsering","suomi"].

surnames() ->
   ["ozcelik","acar","ozgur","ozkan","tez","ustel",
    "vural","akbulut","arslan","avci","ayhan","basturk","caglar","celik","cetinkaya","demir",
    "dikmen","acar","dogan","ekinci","elmas","erdem","erdogan","guler","gunes","ilhan",
    "inan","karaca","karadag","kaya","kemal","keskin","koc","korkmaz","mestafa","osman",
    "ozbek","ozcan","ozdemir","ozden","ozturk","pasa","polat","sezer","sahin","sen",
    "simsek","tekin","tosun","tunc","turan","unal","yalcin","yazici","yildirim","yilmaz"].

ima_gio(N) -> {Id,Name,Surname} = lists:nth(N,imagionary_users()), Id.
ima_gio(N,L) -> {Id,Name,Surname} = lists:nth(N,L), Id.

imagionary_users() ->
    List = [ begin
        [HX|TX] = X, NX = [string:to_upper(HX)] ++ TX,
        [HY|TY] = Y, NY = [string:to_upper(HY)] ++ TY,
        {wf:to_binary(X++"_"++Y),wf:to_binary(NX),wf:to_binary(NY)}
    end || X<-names(), Y<-surnames()],
    lists:keysort(1,List).

fake_id() ->
    FakeUsers = imagionary_users(),
    Pos = crypto:rand_uniform(1, length(FakeUsers)),
    H0 = ima_gio(Pos,FakeUsers),
    Id = wf:to_binary(wf:to_list(H0) ++ wf:to_list(id_generator:get_id2())).

fake_id(Login) -> wf:to_binary(wf:to_list(Login) ++ wf:to_list(id_generator:get_id2())).

create_users(A,B) ->
    ImagioUsers = imagionary_users(),
    [ begin 
        {Id,Name,Surname} = lists:nth(N,ImagioUsers),
        U = #user{  username = Id,
                    id = Id,
                    names = Name,
                    surnames = Surname,
                    birth={1981,9,29} }, kvs:put(U) end || N <- lists:seq(A, B) ].

virtual_users() ->
    case kvs:get(user,<<"maxim@synrc.com">>) of
        {aborted,_} -> kvs:join(), kvs:init_db(),
                create_users(1,100), kvs:put(#user{id= <<"maxim@synrc.com">>});
        _ -> skip end,

    AllUsers = imagionary_users(),
    F = fun({UserId,_,_}, Acc) ->
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
