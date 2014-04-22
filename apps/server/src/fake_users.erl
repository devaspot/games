-module(fake_users).
-compile(export_all).
-include_lib("db/include/config.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("server/include/log.hrl").

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
    FakeUsers = fake_users:imagionary_users(),
    Pos = crypto:rand_uniform(1, length(FakeUsers)),
    H0 = fake_users:ima_gio(Pos,FakeUsers),
    Id = wf:to_binary(wf:to_list(H0) ++ wf:to_list(id_generator:get_id2())).

fake_id(Login) -> wf:to_binary(wf:to_list(Login) ++ wf:to_list(id_generator:get_id2())).
