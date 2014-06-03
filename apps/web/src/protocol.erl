-module(protocol).
-compile({parse_transform, shen}).
-compile(export_all).
-jsmacro([take/2,attach/1,join/1,discard/3,player_info/2,reveal/4,
          piece/2,logout/0,pause/2,i_saw_okey/1,i_have_8_tashes/1]).

attach(Token) -> ws:send(enc(tuple(atom('client'),tuple(atom("session_attach"), Token)))).
join(Game) -> ws:send(enc(tuple(atom('client'),tuple(atom("join_game"), Game)))).
logout() -> ws:send(enc(tuple(atom('client'),tuple(atom("logout"))))).
pause(GameId, Action) -> ws:send(enc(tuple(atom('client'),tuple(atom("pause_game"),atom('undefined'),GameId,atom(Action))))).
player_info(User,GameModule) -> ws:send(enc(tuple(atom('client'),tuple(atom("stats_action"),bin(User),atom(GameModule))))).

take(GameId,Place) -> ws:send(enc(tuple(atom('client'),tuple(atom("game_action"),GameId,atom("okey_take"),[{pile,Place}])))).
discard(GameId, Color, Value) -> ws:send(enc(tuple(atom('client'),tuple(atom("game_action"),GameId,atom("okey_discard"),
    [{tile,tuple(atom("OkeyPiece"), Color, Value)}])))).
piece(Color,Value) -> tuple(atom("OkeyPiece"), Color, Value).
reveal(GameId, Color, Value, Hand) ->
    ws:send(enc(tuple(atom('client'),tuple(atom("game_action"),GameId,atom("okey_reveal"),
        [{discarded, tuple(atom("OkeyPiece"), Color, Value)},{hand, Hand}])))).
i_saw_okey(GameId) -> ws:send(enc(tuple(atom('client'),tuple(atom("game_action"),GameId,atom("okey_i_saw_okey"),[])))).
i_have_8_tashes(GameId) -> ws:send(enc(tuple(atom('client'),tuple(atom("game_action"),GameId,atom("okey_i_have_8_tashes"),[])))).

