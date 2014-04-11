-module(fake_users).
-compile(export_all).
-include_lib("db/include/config.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("server/include/log.hrl").

names() ->
   ["pinar","betul","eda","lale","ilgin","alp","ayberk","vural","ozan","doruk",
    "duman","boran","dursun","taner","uzay","ali","musa","halit","yusuf","isa",
    "asena","aysu","konca","ceren","oylum","filiz","ezgi","ece","sevil","damla",
    "bahar","arzu","dilara","esra","leyla","jale","fatma","irem","yasmin","zeynep",
    "magnolia","jenifer","roksolana","tsering","suomi"].

surnames() ->
   ["ozcelik","acar","ozgur","ozkan","tez","ustel",
    "mehmet","akbulut","arslan","avci","ayhan","basturk","caglar","celik","cetinkaya","demir",
    "dikmen","acar","dogan","ekinci","elmas","erdem","erdogan","guler","gunes","ilhan",
    "inan","karaca","karadag","kaya","kemal","keskin","koc","korkmaz","mestafa","osman",
    "ozbek","ozcan","ozdemir","ozden","ozturk","pasa","polat","sezer","sahin","sen",
    "simsek","tekin","tosun","tunc","turan","unal","yalcin","yazici","yildirim","yilmaz"].

ima_gio(N) -> {MD5,Name} = lists:nth(N,imagionary_users()), Name.
ima_gio(N,L) -> {MD5,Name} = lists:nth(N,L), Name.

imagionary_users() ->
    List = [{crypto:md5(X++Y),X++"_"++Y}||X<-names(),Y<-surnames()],
    lists:keysort(1,List).

