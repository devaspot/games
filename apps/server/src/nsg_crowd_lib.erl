%%% -------------------------------------------------------------------
%%% Author  : Sergei Polkovnikov <serge.polkovnikov@gmail.com>
%%% Description : The library with functions for generation random lists
%%%              of the virtual users
%%% Created : Mar 25, 2013
%%% -------------------------------------------------------------------

-module(nsg_crowd_lib).

-export([virtual_users/0,
         random_users/2]).

virtual_users() ->
    {_, AllUsers} = lists:unzip(nsm_auth:imagionary_users2()),
    F = fun(UserId, Acc) ->
                case auth_server:get_user_info_by_user_id(UserId) of
                    {ok, _} -> [UserId | Acc];
                    {error, _} -> Acc
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
