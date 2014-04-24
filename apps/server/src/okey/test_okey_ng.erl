-module(test_okey_ng).
-compile(export_all).
-include("include/requests.hrl").
-include("include/settings.hrl").
-include_lib("kvs/include/user.hrl").

-define(GAMEID, 1000001).
%-define(GAMEID, 5000220).

main() -> 
    User = #user{id = <<"testbot@bot.net">>, username = <<"testbot">>, surnames = [<<"bottest">>]},

    kvs:add(User),
    PlayerInfo = auth_server:user_info(<<"testbot@bot.net">>),
%%    gas:info(?MODULE, "TEST BOT player info ~p", [PlayerInfo]),

    {ok, BotPid} = game_okey_bot:start_link(self(), PlayerInfo, ?GAMEID),
    erlang:monitor(process, BotPid),

    game_okey_bot:join_game(BotPid),
    
    loop(BotPid).

loop(BotPid) ->
    gas:info("TEST OKEY NG listen to air"),
    
    receive 
        {'DOWN', _MonitorRef, _Type, _Object, _Info} -> ok;
        UnknowMsg -> wf:info("Unknow message: ~p", [UnknowMsg]), loop(BotPid)
    end.
