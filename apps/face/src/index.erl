-module(index).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("../../server/include/requests.hrl").
-include("../../server/include/settings.hrl").
-include_lib("avz/include/avz.hrl").
-include_lib("kvs/include/user.hrl").
-jsmacro([take/2,attach/1,join/1,discard/3,player_info/2,reveal/4,piece/2,logout/0,pause/2]).

-define(GAMEID, 1000001).

user() -> 
    case wf:user() of undefined ->
        Imagionary = fake_users:imagionary_users(),
        {Id,Name,Surname} = lists:nth(crypto:rand_uniform(1,length(Imagionary)),Imagionary),
        X = #user{id = fake_users:fake_id(Id),
                  names = Name,
                  surnames = Surname},
        wf:user(X), X; U-> U end.

join(Game) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("join_game"), Game)))).

logout() ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("logout"))))).

attach(Token) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("session_attach"), Token)))).

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

color(Id,Color) -> wf:wire(wf:f("document.querySelector('#~s').style.color = \"~s\";",[Id,Color])).
unselect(Id) -> color(Id,black).
select(Id) -> color(Id,red).

redraw_tiles(undefined) -> [];
redraw_tiles([{Tile, _}| _ ] = TilesList) ->
    wf:update(discard_combo,
        [#dropdown{id = discard_combo, postback = combo,
        value = Tile, source = [discard_combo],
        options = [#option{label = CVBin, value = CVBin} || {CVBin, _} <- TilesList]}]).

redraw_players(Players) ->
    User = user(),
    [ begin PN = player_name(PI),
        wf:update(LabelId, [#label{id = LabelId,
        style= case User#user.id == Id of
            true -> "font-weight: bold;";
            _ -> "" end, body = <<" ",PN/binary>>}]) 
     end || {LabelId, _, #'PlayerInfo'{id=Id}=PI} <- Players].

player_name(PI) -> auth_server:player_name(PI).

main() -> #dtl{file="index", bindings=[{title,<<"N2O">>},{body,body()}]}.

body() ->
    wf:wire(#api{name=plusLogin, tag=plus}),
    [ #panel{ id=history },
      #button{ id = plusloginbtn, body = <<"Login">>, postback=login_button},
      #br{},

%%      #dropdown{ id = p1right_combo, body = "0 0"},
      #label{ id = player1, body = "Player 1", style = "color=black;"},

%%      #dropdown{ id = p2right_combo, body = "0 0"},
      #label{ id = player2, body = "Player 2", style = "color=black;"},

%%      #dropdown{ id = p3right_combo, body = "0 0"},
      #label{ id = player3, body = "Player 3", style = "color=black;"},

%%      #dropdown{ id = p4right_combo, body = "0 0"},
      #label{ id = player4, body = "Player 4", style = "color=black;"},

      #br{},
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #dropdown{ id= take_combo, value="0",
                 options = 
                     [
                      #option { label= <<"0">>, value= <<"0">> },
                      #option { label= <<"1">>, value= <<"1">> }
                     ]
               },
      #button{ id=take, body = <<"Take">>, postback = take, source=[take_combo]},
      #dropdown{ id=discard_combo, value="2", postback=combo, source=[discard_combo], 
                 options = 
                     [
                      #option { label= <<"Option 1">>, value= <<"1">> },
                      #option { label= <<"Option 2">>, value= <<"2">> },
                      #option { label= <<"Option 3">>, value= <<"3">> }
                     ]
               },
      #button{ id = discard, body = <<"Discard">>, postback = discard, source=[discard_combo]},
      #button{ id = reveal, body = <<"Reveal">>, postback = reveal, source = [discard_combo]},
      #button{ id = pause, body = <<"Pause">>, postback = pause},
      #button{ id = player_info, body = <<"PlayerInfo">>, postback = player_info}
    ].

event(terminate) -> wf:user(undefined), wf:info("terminate");
event(init) -> event(attach), event(join);

event(combo)  -> wf:info("Combo: ~p",[wf:q(discard_combo)]);
event(join)   -> wf:wire(join(wf:to_list(?GAMEID)));
event(take)   -> wf:wire(take(wf:to_list(?GAMEID), wf:q(take_combo)));

event(player_info) -> 
    User = user(),
    wf:wire(player_info(wf:f("'~s'",[wf:to_list(User#user.id)]),wf:f("'~s'",[game_okey])));

event(attach) -> 
    wf:info("ATTACH"),
    {ok,GamePid} = game_session:start_link(self()),
    wf:session(<<"game_pid">>,GamePid),
    User = user(),
    put(okey_im, User),
    wf:info("Session User: ~p",[User]),
    Token = auth_server:generate_token(?GAMEID,User),
    wf:wire(attach(wf:f("'~s'",[Token]))),
    ok;

event(discard) -> 
    TilesList = get(game_okey_tiles),
    DiscardCombo = wf:q(discard_combo),
    case lists:keyfind(erlang:list_to_binary(DiscardCombo), 1, TilesList) of
    {_, {C, V}} ->
        wf:wire(discard(wf:to_list(?GAMEID), erlang:integer_to_list(C), erlang:integer_to_list(V)));
    false -> wf:info("Discard Combo: ~p",[DiscardCombo]) end;

%event({binary,M}) -> {ok,<<"Hello">>};

event({client,Message}) ->
    case wf:session(<<"game_pid">>) of
        undefined -> skip;
        GamePid -> game_session:process_request(GamePid, Message) end;

event({server, {game_event, _, okey_game_started, Args}}) ->
    {_, Tiles} = lists:keyfind(tiles, 1, Args),
    TilesList = [{wf:to_binary([wf:to_list(C)," ",wf:to_list(V)]), {C, V}} || {_, C, V} <- Tiles],
    %%wf:info("tiles ~p", [TilesList]),
    put(game_okey_tiles, TilesList),
    put(game_okey_pause, resume),
    redraw_tiles(TilesList);

event({server, {game_event, _, okey_game_player_state, Args}}) ->

    {_, WhosMove} = lists:keyfind(whos_move, 1, Args),
    Players = get(okey_players),
    {X, _, _} = lists:keyfind(WhosMove, 2, Players),
    case X of
        null -> skip;
        false -> skip;
        X -> select(X), put(okey_turn_mark,X) end,

    {_, Tiles} = lists:keyfind(tiles, 1, Args),
    TilesList = [{wf:to_binary([wf:to_list(C)," ",wf:to_list(V)]),{C, V}}|| {_, C, V} <- Tiles],
    redraw_tiles(TilesList),
    put(game_okey_tiles, TilesList);

event({server, {game_event, _, okey_tile_taken, Args}}) ->
    Im = get(okey_im),
    {_, Player} = lists:keyfind(player, 1, Args),
    if
       Im == Player ->
            case lists:keyfind(revealed, 1, Args) of
                {_, {_, C, V}} ->
                    TilesList = [{wf:to_binary([wf:to_list(C), " ", wf:to_list(V)]), {C, V}} | get(game_okey_tiles)],
                    put(game_okey_tiles, TilesList),
                    redraw_tiles(TilesList);
                _ -> ok end;
       true -> ok end;

event({server, {game_event, _, okey_tile_discarded, Args}}) ->
    Im = get(okey_im),
%%    Players = get(okey_players),
    {_, Player} = lists:keyfind(player, 1, Args),
%%    wf:info("++++ ~p", [Args]),

    if
       Im == Player ->
            {_, {_, C, V}} = lists:keyfind(tile, 1, Args),
            TilesListOld = get(game_okey_tiles),
            TilesList = lists:keydelete({C, V}, 2, TilesListOld),
            put(game_okey_tiles, TilesList),
            redraw_tiles(TilesList);
       true ->
            ok
    end;


event({server,{game_event, Game, okey_turn_timeout, Args}}) ->
    wf:info("okey_turn_timeout ~p", [Args]),
    {_, TileTaken} = lists:keyfind(tile_taken, 1, Args),
    event({server, {game_event, Game, okey_tile_taken,
            [{player, get(okey_im)}, {revealed, TileTaken}]}}),
    {_, TileDiscarded} = lists:keyfind(tile_discarded, 1, Args),
    event({server, {game_event, Game, okey_tile_discarded,
            [{player, get(okey_im)}, {tile, TileDiscarded}]}});

event({server, {game_event, _, okey_game_info, Args}}) ->
%%  wf:info("okay_game_info ~p", [Args]),
    {_, PlayersInfo} = lists:keyfind(players, 1, Args),

%%  [p1right_combo, p2right_combo, p3right_combo],

    Players = 
        lists:zipwith(
          fun(LabelId, #'PlayerInfo'{id = Id}=PI) -> {LabelId, Id, PI} end,
          [player1, player2, player3, player4], PlayersInfo),
    put(okey_players, Players),

%   wf:info("players ~p", [Players]),

%%  put(pkey_game_right, [{p1right_combo, []}, {p2right_combo, []}, {p3right_combo, []}]),
    redraw_players(Players);

event({server,{game_event, _, player_left, Args}}) ->
    {_, OldPlayerId} = lists:keyfind(player, 1, Args),
    {_, PI} = lists:keyfind(replacement, 1, Args),
    #'PlayerInfo'{id = NewPlayerId, robot = IsRobot} = PI,
    OldPlayers = get(okey_players),

    {ListId, _, _} = lists:keyfind(OldPlayerId, 2, OldPlayers),

    NewPlayers = lists:sort(fun({E1, _, _}, {E2, _, _}) -> E1 < E2 end,
        [{ListId, NewPlayerId, PI} | lists:keydelete(OldPlayerId, 2, OldPlayers)]),

    put(okey_players, NewPlayers),
    redraw_players(NewPlayers),

    case get(okey_turn_mark) of undefined -> ok; X -> select(X) end;

event({server,{game_event, _, okey_next_turn, Args}}) ->
    {player, PlayerId} = lists:keyfind(player, 1, Args),
%    wf:info("im ~p next turn players ~p ~p", [get(okey_im), PlayerId, get(okey_players)]),
    {LabelId, _, _} = lists:keyfind(PlayerId, 2, get(okey_players)),
    case get(okey_turn_mark) of
        undefined -> ok;
        OldLabelId -> unselect(OldLabelId) end,
    select(LabelId),
    put(okey_turn_mark, LabelId);

event(reveal) ->
    TilesList = get(game_okey_tiles),
    Discarded = wf:q(discard_combo),

    case lists:keyfind(wf:to_binary(Discarded), 1, TilesList) of
        {_, {CD, VD} = Key} ->
            Hand = [{C,V} || {_, {C, V}} <- lists:keydelete(Key, 2, TilesList) ],
            HandJS = "[[" ++ string:join([
                wf:f("bert.tuple(bert.atom('OkeyPiece'),~p,~p)",[C,V]) || {C,V} <- Hand],",") ++ "],[]]",
            RevealJS = reveal(wf:to_list(?GAMEID),wf:f("~p",[CD]),wf:f("~p",[VD]),HandJS),
            wf:info("RevealJS: ~p",[lists:flatten(RevealJS)]),
            wf:wire(RevealJS);
        _ ->
            wf:info("error discarded ~p", Discarded)
    end;

event(login_button) -> wf:wire(logout());
event({register,User}) -> wf:info("Register: ~p",[User]), kvs:add(User), wf:user(User);
event({login,User}) -> wf:info("Login: ~p",[User]), kvs:put(User), wf:user(User);

event(pause) ->
    Action  =
        case get(game_okey_pause) of 
            X when X == resume orelse X == undefined -> 
                put(game_okey_pause, pause),
                wf:update(pause, [#button{id = pause, body = "Resume", postback = pause}]),
                "pause";
            pause ->
                put(game_okey_pause, resume),
                wf:update(pause, [#button{id = pause, body = <<"Pause">>, postback = pause}]),
                "resume"
        end,
    wf:wire(pause(wf:to_list(?GAMEID), wf:f("~p", [Action])));

event(Event)  -> ok. %wf:info("Event: ~p", [Event]).

api_event(X,Y,Z) -> avz:api_event(X,Y,Z).
