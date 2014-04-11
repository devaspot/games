-module(nsm_auth).
-compile(export_all).
-include_lib("db/include/config.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("db/include/user_info.hrl").
-include_lib("server/include/log.hrl").

login(Data) ->
    UserName = proplists:get_value(username, Data),
    Password = proplists:get_value(password, Data),
    HashedPassword = utils:sha(Password),

    Reply =
        case kvs:get(user,UserName) of
            {ok, #user{password = HashedPassword, username = U} = User } ->
                case User#user.status of
                    ok ->
%                        nsm_users:init_mq_for_user(UserName),
%                        nsx_msg:notify([user, init], User),
                        {ok, U};
                    not_verified ->
                        {error, not_verified};
                    banned ->
                        {error, banned};
                    _ ->
                        {error, unknown}
                end;
            {ok, _} ->
                {error, incorrect_password};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found}
        end,
    Reply.

get_user_info(UserId) ->
    Reply =
        case kvs:get(user,UserId) of
            {ok, User} ->
                UserInfo = build_user_info(User),
                {ok, UserInfo};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found};
            {aborted, _} ->
                {error, user_not_found}
        end,
    Reply.

get_all_user() -> {ok, kvs:all(user)}.

generate_token(User) ->
    Token = generate_token0(),
    Res = auth_server:store_token(0, Token, User),
    gs:info("with result :~p~n", [Res]),
    Token.

generate_token0() -> T0 = crypto:rand_bytes(100), T = base64:encode(T0), T.

build_user_info(#user{username = UserName,
                      id = Name,
                      surnames = Surname,
                      birth = Age,
                      sex = Sex} = User) ->
    #user_info{username = Name,
               name = Name,
               surname = Surname,
               age = Age,
               avatar_url = get_avatar(User, small),
               sex = Sex}.

get_avatar(#user{avatar = Avatar}, Size) -> get_avatar(Avatar, Size);
get_avatar(Avatar, Size) ->
    case Size of
         big -> "/images/no_avatar_big.jpg";
         small -> "/images/no_avatar_small.jpg";
         tiny -> "/images/no_avatar_tiny.jpg" end.

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

