-module(protocol).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-jsmacro([take/2,attach/1,join/1,discard/3,player_info/2,reveal/4,piece/2,logout/0,pause/2]).

attach(Token) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("session_attach"), Token)))).

join(Game) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("join_game"), Game)))).

logout() ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("logout"))))).

take(GameId,Place) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("game_action"),GameId,
            bert:atom("okey_take"),[{pile,Place}])))).

discard(GameId, Color, Value) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(
            bert:atom("game_action"),
            GameId,
            bert:atom("okey_discard"),
            [{tile, bert:tuple(bert:atom("OkeyPiece"), Color, Value)}])))).

player_info(User,GameModule) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("get_player_stats"),bert:binary(User),bert:atom(GameModule))))).

piece(Color,Value) ->
    bert:tuple(bert:atom("OkeyPiece"), Color, Value).

reveal(GameId, Color, Value, Hand) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(
            bert:atom("game_action"),
            GameId,
            bert:atom("okey_reveal"),
            [{discarded, bert:tuple(bert:atom("OkeyPiece"), Color, Value)},
             {hand, Hand}])))).

pause(GameId, Action) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom("client"),
        bert:tuple(
            bert:atom("pause_game"),
            bert:atom("undefined"),
            GameId,
            bert:atom(Action))))).
