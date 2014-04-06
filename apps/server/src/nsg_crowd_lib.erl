%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The library with functions for generation random lists
%%%              of the virtual users
%%% Created : Mar 25, 2013
%%% -------------------------------------------------------------------

-module(nsg_crowd_lib).
-include_lib("kvs/include/user.hrl").

-compile(export_all).

create_users(A,B) ->
    ImagioUsers = nsm_auth:imagionary_users(),
    [ begin U = #user{username = nsm_auth:ima_gio(N,ImagioUsers),
                            id = nsm_auth:ima_gio(N,ImagioUsers),
                            birth={1981,9,29}
                           }, kvs:put(U) end || N <- lists:seq(A, B) ].

virtual_users() ->
    case kvs:get(user,"maxim@synrc.com") of
        {aborted,_} -> kvs:join(), kvs:init_db(),
                create_users(1,100), kvs:put(#user{id="maxim@synrc.com"});
        {ok,_} -> skip end,

    {_, AllUsers} = lists:unzip(nsm_auth:imagionary_users()),
    F = fun(UserId, Acc) ->
        User = auth_server:get_user_info_by_user_id(UserId),
        case User of
                    {ok, _} -> [UserId | Acc];
                    {error,_} -> Acc
                end
        end,
    lists:usort(lists:foldl(F, [], AllUsers)).

random_users(Num, AllUsers) ->
    AllUsersNum = length(AllUsers),
    random_users(Num, [], AllUsers, AllUsersNum).

random_users(0, Acc, _AllUsers, _AllUsersNum) -> Acc;
random_users(N, Acc, AllUsers, AllUsersNum) ->
    User = list_to_binary(lists:nth(crypto:rand_uniform(1, AllUsersNum + 1), AllUsers)),
    case lists:member(User, Acc) of
        false -> random_users(N - 1, [User | Acc], AllUsers, AllUsersNum);
        true -> random_users(N, Acc, AllUsers, AllUsersNum)
    end.