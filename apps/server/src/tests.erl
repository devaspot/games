-module(tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("server/include/conf.hrl").
-include_lib("server/include/log.hrl").
-include_lib("server/include/kamf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/classes.hrl").
-include_lib("server/include/game_okey.hrl").

-export([setup/0, cleanup/1, tests_on/0]).

-define(BT, 3000000).

-define(SIM_TIMEOUT, 300).

%% ===================================================================
%% ===================================================================
%% Tests
%% ===================================================================
%% ===================================================================

stateful_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     ?_test(
        begin
            send_receive_object_t(),
            policy_file_plain_t(),
            policy_file_t(),
            policy_file_complex_t(),
            async_server_rpc_test_t(),
            encoding_client_failure_t(),
            ok
        end)
    }.

session_attach_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     ?_test(
        begin
            session_attach_fail_t(),
            session_attach_ok_t(),
            ok
        end)
    }.

message_read_write_test() ->
    A = #game_event{event = <<"game_ended">>, game = 1, args = [{good_shot, true}, {awarded_points, 42}, {reason, <<"out_of_toshes">>}, {results, #'OkeyGameResults'{}}]},
    {A, A} = {A, ?AMF:object_to_record(?AMF:record_to_object(A))}.

setup() ->
    ok = app_util:start(kakaconfig),
    kakaconfig:temp_db(),
    meck:new(kakaconfig, [passthrough]),
    meck:expect(kakaconfig, get, fun
                                     ([debug, is_test], _) -> {ok, true};
                                     (okey_robot_delay, _) -> {ok, 1};
                                     (Key, X) -> meck:passthrough([Key, X])
                                 end),

    meck:new(okey_stats, [passthrough]),
    meck:expect(okey_stats, add_game, fun(R) -> ?INFO("saving game: ~p", [R]), {1, 1} end),
    meck:expect(okey_stats, get_skill, fun(_) -> {ok, 500} end),
    meck:expect(okey_stats, get_game_points, fun(_, _) -> {ok, [{game_points, 100},
                                                                {finished_with_okey, 5},
                                                                {finished_with_8_tashes, 1}]} end),
    ok = app_util:start(kaka_id_generator),
    ok = app_util:start(kaka_utils),
    ok = app_util:start(kaka_auth_server),
    ok = app_util:start(kaka_game_session),
    ok = app_util:start(kaka_game_manager),
    ok = app_util:start(kaka_matchmaking),
    ok = app_util:start(kaka_conn).

cleanup(_State) ->
    application:stop(kaka_conn),
    application:stop(kaka_matchmaking),
    application:stop(kaka_game_manager),
    application:stop(kaka_game_session),
    application:stop(kaka_auth_server),
    application:stop(kaka_utils),
    application:stop(kaka_id_generator),
    meck:unload(kakaconfig),
    meck:unload(okey_stats),
    db:stop(),
    ok.

tests_on() ->
    meck:new(kakaconfig, [passthrough]),
    meck:expect(kakaconfig, get, fun
                                     ([debug, is_test], _) -> {ok, true};
                                     (Key, X) -> meck:passthrough([Key, X])
                                 end).

session_attach_fail_t() ->
    Token = "some fake token",
    S1 = tc:connect(),
    {error, <<"invalid_token">>} = tc:call_rpc(S1, #session_attach{token = Token}).

session_attach_ok_t() ->
    Token = "some proper token",
    UserId = "mr_customer",
    auth_server:store_token(Token, UserId),
    S1 = tc:connect(),
    Res = tc:call_rpc(S1, #session_attach{token = Token}),
    #'PlayerInfo'{id = Id, login = Login} = Res,
    true = is_binary(Login),
    true = is_binary(Id),
    tc:call_rpc(S1, #logout{}).

encoding_client_failure_t() ->
    Socket = tc:connect(),
    Answer = tc:call_rpc_raw(Socket, 'getobjecttypefromserver_zzz', []),
    ?INFO("encoding_client_failure_t: ~p", [Answer]),
    {error, _Reason} = Answer.



async_server_rpc_test_t() ->
    TestClient = spawn_link(fun() -> tc:async_test_client_loop() end),
    timer:sleep(100),
    [{_, Pid, _, _}] = nsm_conn_app:get_clients(),

    Self = self(),
    Ref = make_ref(),
    spawn(fun() ->
                  Msg = #slowping{},
                  <<"slowpong">> = conn_kamf_worker:send_request(Pid, Msg),
                  Self ! Ref
          end),

    Message = #fastping{},
    <<"fastpong">> = conn_kamf_worker:send_request(Pid, Message),
    receive
        Ref -> ok
    after 5000 -> erlang:error(timeout) end,

    TestClient ! exit.

message_embedding_test() ->
    A = {object,<<"KamfResponse">>,
         [{id,1001},
          {result,{object,
                   <<"PlayerInfo">>,
                   [{login,"oldman"},
                    {name,"Ernest"},
                    {surname,"Hemingway"}]}}]},
    Bin = ?AMF:encode(A),
    {A, _} = ?AMF:decode(Bin).

kamf_conversion_test() ->
    AR = {object,
          <<"KamfRequest">>,
          [{id,0},
           {method,
            <<"session_attach">>},
           {args,
            [{<<"token">>,
              "EBAs6dg2Xw6XuCdg8qiPmlBLgYJ6N4Ti0P+oGpWgYz4NW4nBBUzTe/wAuLYtPnjFpsjCExxSpV78fipmsPxcf+NGy+QKIM6rmVJhpnIlKf0bpFNuGaAPjZAWthhGO8nZ0V8UnA=="}]}]},
    BR = {object,
          <<"KamfRequest">>,
          [{id,0},
           {method,
            <<"session_attach">>},
           {args,
            [{<<"token">>,
              <<"EBAs6dg2Xw6XuCdg8qiPmlBLgYJ6N4Ti0P+oGpWgYz4NW4nBBUzTe/wAuLYtPnjFpsjCExxSpV78fipmsPxcf+NGy+QKIM6rmVJhpnIlKf0bpFNuGaAPjZAWthhGO8nZ0V8UnA==">>}]}]},
    kamf:object_to_record(AR),
    kamf:object_to_record(BR),
    ok.

send_receive_object_t() ->
    Socket = tc:connect(),
    L = #'some_named_object'{name1 = <<"value1">>, name2 = 2, name3 = 3.0},
    R = tc:call_rpc(Socket, #'getobjecttypefromserver'{}),
    {L, L} = {L, R},
    <<"Hello, guys! Have a nice day :)">> = tc:call_rpc(Socket, #'getstringtypefromserver'{}),
    42 = tc:call_rpc(Socket, #'getintegertypefromserver'{}),
    [_, {some_number, 42}, _, _] = tc:call_rpc(Socket, #'getmixedtypesfromserver'{}),
    tc:close(Socket).

policy_file_t() ->
    {ok, Socket} = gen_tcp:connect(localhost, ?LISTEN_PORT, [{active, false}, binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<"<policy-file-request/>", 0>>),
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Data = conn_kamf_worker:policy_file_text(),
    ok = gen_tcp:send(Socket, <<?KAMF_MAGIC:48>>),
    Sock = tc:connect(Socket),
    #'some_named_object'{name1 = <<"value1">>, name2 = 2, name3 = 3.0} =
        tc:call_rpc(Sock, #'getobjecttypefromserver'{}),
    ok = tc:close(Sock).

policy_file_complex_t() ->
    {ok, Socket} = gen_tcp:connect(localhost, ?LISTEN_PORT, [{active, false}, binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<"<policy-file-request/>", 0, ?KAMF_MAGIC:48>>),
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Data = conn_kamf_worker:policy_file_text(),
    Sock = tc:connect(Socket),
    #'some_named_object'{name1 = <<"value1">>, name2 = 2, name3 = 3.0} =
        tc:call_rpc(Sock, #'getobjecttypefromserver'{}),
    ok = tc:close(Sock).


policy_file_plain_t() ->
    {ok, Socket} = gen_tcp:connect(localhost, ?LISTEN_PORT, [{active, false}, binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, <<"<policy-file-request/>", 0>>),
    {ok, Data} = gen_tcp:recv(Socket, 0),
    Data = conn_kamf_worker:policy_file_text(),
    ok = gen_tcp:close(Socket).

