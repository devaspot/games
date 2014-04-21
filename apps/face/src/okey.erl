-module(okey).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("../../server/include/requests.hrl").
-include("../../server/include/settings.hrl").
-include_lib("avz/include/avz.hrl").
-include_lib("kvs/include/user.hrl").

-define(GAMEID, 1000001).
%-define(GAMEID, 5000220).

-record(player, {id, label, info, take, discard, history = []}).

-define(RESET_ELEMENTS, [
    {gosterge, #label{ id = gosterge, body="Gosterge: "}},
    {h1, #dropdown{ id = h1, options = []}},
    {h2, #dropdown{ id = h2, options = []}},
    {h3, #dropdown{ id = h3, options = []}},
    {h4, #dropdown{ id = h4, options = []}} ]).

user() -> 
    case wf:user() of undefined ->
        Imagionary = fake_users:imagionary_users(),
        {Id,Name,Surname} = lists:nth(crypto:rand_uniform(1,length(Imagionary)),Imagionary),
        X = #user{id = fake_users:fake_id(Id),names = Name,surnames = Surname},
        wf:user(X), X; U-> U end.

color(Id,Color) -> wf:wire(wf:f("document.querySelector('#~s').style.color = \"~s\";",[Id,Color])).
unselect(Id) -> color(Id,black).
select(Id) -> color(Id,red).

redraw_istaka(TilesList) ->
    redraw_tiles(TilesList, #dropdown{id = istaka, postback = combo, source = [istaka]}).

redraw_tiles(undefined, _DropDown) -> [];
redraw_tiles([] = _TilesList, DropDown = #dropdown{id = ElementId}) ->
    wf:update(ElementId, [DropDown#dropdown{value = [], options = []}]);
redraw_tiles([{Tile, _}| _ ] = TilesList, DropDown = #dropdown{id = ElementId}) ->
    wf:update(ElementId, [DropDown#dropdown{value = Tile, options = [#option{label = CVBin, value = CVBin} || {CVBin, _} <- TilesList]}]).

redraw_players(Players) ->
    User = user(),
    [ begin PN = player_name(PI),
            wf:update(LabelId, #label{ id = LabelId,
               style= case User#user.id == Id of
                  true -> "font-weight: bold;";
                  _ -> "" end, body = <<" ",PN/binary," ">>}) 
      end || #player{label = LabelId, info =  #'PlayerInfo'{id = Id} = PI} <- Players].

update_players(UpdatedPlayer = #player{label = LabelId}, Players) ->
    lists:sort(
      fun(#player{label = E1}, #player{label = E2}) -> E1 < E2 end,
      [UpdatedPlayer | lists:keydelete(LabelId, #player.label, Players)]
     ).

player_name(PI) -> auth_server:player_name(PI).

tash(C,V) -> {wf:to_binary([wf:to_list(C)," ",wf:to_list(V)]), {C, V}}.

main() -> #dtl{file="index", bindings=[{title,<<"N2O">>},{body,body()}]}.

body() ->
    wf:wire(#api{name=plusLogin, tag=plus}),

  [ #panel    { id = history },
    #button   { id = pluslogin,  body = "Login",       postback = login_button },
    #label    { id = nothing,    body = " Google"},    #br{}, #br{},
    #label    { id = gosterge,   body = "Gosterge"},   #br{},
    #label    { id = player1,    body = "Seat 1"},     #dropdown{id=h1,options=[]}, #br{},
    #label    { id = player2,    body = "Seat 2"},     #dropdown{id=h2,options=[]}, #br{},
    #label    { id = player3,    body = "Seat 3"},     #dropdown{id=h3,options=[]}, #br{},
    #label    { id = player4,    body = "Seat 4"},     #dropdown{id=h4,options=[]}, #br{}, #br{},
    #button   { id = attach,     body = "Attach",      postback = attach },
    #button   { id = join,       body = "Join",        postback = join, source = [games_ids]},
    #dropdown { id = games_ids,  postback = combo,     options = []}, #br{},
    #dropdown { id = take_src,                         options = [
                                                            #option{label="Table",value="0"},
                                                            #option{label="Left",value="1"}]},
    #button   { id = take,       body = "Take",        postback = take,    source=[take_src]},
    #dropdown { id = istaka,                           postback = combo,   source=[istaka],options=[]},
    #button   { id = discard,    body = "Discard",     postback = discard, source=[istaka]},
    #button   { id = reveal,     body = "Reveal",      postback = reveal,  source=[istaka]}, #br{},
    #button   { id = saw_okey,   body = "I Saw Okey",  postback = i_saw_okey},
    #button   { id = have_8,     body = "8 Tashes",    postback = i_have_8_tashes},
    #button   { id = pause,      body = "Pause",       postback = pause},
    #button   { id = info,       body = "PlayerInfo",  postback = player_info} ].

event(terminate) -> wf:info("terminate");

event(init) -> 
    GamesIds = case game_manager:get_all_games_ids() of
      [] -> [?GAMEID];
      List -> List end,
    
    wf:update(games_ids,#dropdown{id = games_ids, value = ?GAMEID, options = 
      [#option{label = wf:to_list(GameId), value = wf:to_list(GameId)} || GameId <- GamesIds]}),
    
    event(attach),
    event(join);

event(login_button) -> wf:wire(protocol:logout());
event(join) -> 
    GameId = get(okey_game_id),
    wf:wire(protocol:join(wf:to_list(GameId)));
event(take) -> 
    GameId = get(okey_game_id),
    wf:wire(protocol:take(wf:to_list(GameId), wf:q(take_src)));

event(player_info) -> 
    User = user(),
    wf:wire(protocol:player_info(
        wf:f("'~s'",[wf:to_list(User#user.id)]),wf:f("'~s'",[game_okey])));

event(attach) -> 
    {ok,GamePid} = game_session:start_link(self()),
    wf:session(<<"game_pid">>,GamePid),
    User = user(),
    put(okey_im, User#user.id),
    wf:wire(wf:f("document.user = '~s';",[User#user.id])),
    wf:info("Session User: ~p",[User]),
    GameId = case wf:q(games_ids) of undefined -> ?GAMEID; Res -> Res end,
    put(okey_game_id, GameId),
    Token = auth_server:generate_token(GameId,User),
    wf:wire(protocol:attach(wf:f("'~s'",[Token]))),
    ok;

event(discard) -> 
    TilesList = get(game_okey_tiles),
    DiscardCombo = wf:q(istaka),
    GameId = get(okey_game_id),
    case lists:keyfind(erlang:list_to_binary(DiscardCombo), 1, TilesList) of
    {_, {C, V}} ->
        wf:wire(protocol:discard(wf:to_list(GameId), wf:to_list(C), wf:to_list(V)));
    false -> wf:info("Discard Combo: ~p",[DiscardCombo]) end;

event(reveal) ->
    TilesList = case get(game_okey_tiles) of undefined -> []; T -> T end,
    Discarded = wf:q(istaka),
    GameId = get(okey_game_id),

    case lists:keyfind(wf:to_binary(Discarded), 1, TilesList) of
        {_, {CD, VD} = Key} ->
            Hand = [{C,V} || {_, {C, V}} <- lists:keydelete(Key, 2, TilesList) ],
            HandJS = "[[" ++ string:join([
                wf:f("tuple(atom('OkeyPiece'),~p,~p)",[C,V]) || {C,V} <- Hand],",") ++ "],[]]",
            RevealJS = protocol:reveal(wf:to_list(GameId),wf:f("~p",[CD]),wf:f("~p",[VD]),HandJS),
            wf:info("RevealJS: ~p",[lists:flatten(RevealJS)]),
            wf:wire(RevealJS);
        _ ->
            wf:info("error discarded ~p", Discarded)
    end;

event(i_saw_okey) ->
    wf:info("i_saw_okey!"),
    GameId = get(okey_game_id),
    wf:wire(protocol:i_saw_okey(wf:to_list(GameId)));

event(i_have_8_tashes) ->
    wf:info("i_gave_8_tashes!"),
    GameId = get(okey_game_id),
    wf:wire(protocol:i_have_8_tashes(wf:to_list(GameId)));

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
    GameId = get(okey_game_id),
    wf:wire(protocol:pause(wf:to_list(GameId), wf:f("~p", [Action])));

%event({binary,M}) -> {ok,<<"Hello">>};

event({client,Message}) ->
    wf:info("Client: ~p", [Message]),
    case wf:session(<<"game_pid">>) of
        undefined -> skip;
        GamePid -> SyncRes = game_session:process_request(GamePid, Message),
                   wf:info("Sync Result: ~p",[SyncRes]) end;

event({server, {game_event, _, okey_game_started, Args}}) ->
    wf:info("Game Started: ~p", [Args]),
    {_, Tiles} = lists:keyfind(tiles, 1, Args),
    TilesList = [tash(C, V) || {_, C, V} <- Tiles],
    case lists:keyfind(gosterge, 1, Args) of
        {_, {_, C, V}} ->
            wf:update(gosterge, #label{id = gosterge, body = wf:to_binary(["Gosterge: ", wf:to_list(C), " ", wf:to_list(V)])});
        _ -> ok end,
    put(game_okey_tiles, TilesList),
    put(game_okey_pause, resume),
    redraw_istaka(TilesList);

event({server, {game_event, _, okey_game_player_state, Args}}) ->
    wf:info("Player State: ~p", [Args]),
    case lists:keyfind(whos_move, 1, Args) of 
        {_, null} -> ok;
        {_, WhosMove} ->
            Players = get(okey_players),

            #player{label = X} = lists:keyfind(WhosMove, #player.id, Players),
            case X of
                null -> skip;
                false -> skip;
                X -> select(X), put(okey_turn_mark,X) end,

            case lists:keyfind(gosterge, 1, Args) of
                {_, {_, C, V}} ->
                    wf:update(gosterge, #label{id = gosterge, body = wf:to_binary(["Gosterge: ", wf:to_list(C), " ", wf:to_list(V)])});
                _ -> ok end,

            {_, Tiles} = lists:keyfind(tiles, 1, Args),
            TilesList = [tash(C, V)|| {_, C, V} <- Tiles],
            redraw_istaka(TilesList),
            put(game_okey_tiles, TilesList),

            {_, Piles} = lists:keyfind(piles, 1, Args),

            UpdatedPlayers = [ begin

                Player = #player{discard = RightPileComboId}
                       = lists:keyfind(PlayerId, #player.id, Players),

                ConvertedPile = [ tash(C, V) || {_, C, V} <- Pile],

                redraw_tiles(ConvertedPile, #dropdown{id = RightPileComboId}),
                Player#player{history = ConvertedPile}

            end || {PlayerId, Pile} <- Piles ],

            put(okey_players, lists:sort(fun(#player{label = E1}, #player{label = E2}) ->
                                E1 < E2 end, UpdatedPlayers));
       _ -> ok end;

event({server, {game_event, _, okey_tile_taken, Args}}) ->
    wf:info("Taken: ~p", [Args]),
    Im = get(okey_im),

    {_, PlayerId} = lists:keyfind(player, 1, Args),
    case lists:keyfind(revealed, 1, Args) of
        {_, {_, C, V}} ->
            if  Im == PlayerId ->
                TilesList = [ tash(C, V) | get(game_okey_tiles)],
                %%wf:info("Tiles: ~p",[TilesList]),
                put(game_okey_tiles, TilesList),
                redraw_istaka(TilesList);
                true -> ok end,
            case lists:keyfind(pile, 1, Args) of
                {_, 1} -> %% have taken from left
                    Players = get(okey_players),

                    #player{take=From} = lists:keyfind(PlayerId,#player.id,Players),
                    LeftPlayer = #player{discard=Combo,history=DiscardHistory}
                               = lists:keyfind(From,#player.label,Players),

                    History = lists:keydelete({C, V}, 2, DiscardHistory),
                    redraw_tiles(History, #dropdown{id=Combo}),
                    put(okey_players,update_players(LeftPlayer#player{history=History},Players));

                _ -> ok end;
       _ -> ok end;

event({server, {game_event, _, okey_tile_discarded, Args}}) ->
    wf:info("Discarded: ~p", [Args]),
    Im = get(okey_im),
    {_, PlayerId}  = lists:keyfind(player, 1, Args),
    {_, {_, C, V}} = lists:keyfind(tile, 1, Args),

    if  Im == PlayerId ->
        TilesListOld = get(game_okey_tiles),
        TilesList = lists:keydelete({C, V}, 2, TilesListOld),
        put(game_okey_tiles, TilesList),
        redraw_istaka(TilesList);
        true -> ok end,

    Players = get(okey_players),
    Player  = #player{discard = RightPileComboId, history = OldRightPile}
            = lists:keyfind(PlayerId, #player.id, Players),

    NewRightPile = [ tash(C, V) | OldRightPile],
    redraw_tiles(NewRightPile, #dropdown{id = RightPileComboId}),
    UpdatedPlayer = Player#player{history = NewRightPile},
    UpdatedPlayers = update_players(UpdatedPlayer, Players),
    put(okey_players, UpdatedPlayers);

event({server,{game_event, _Game, okey_turn_timeout, Args}}) ->
    wf:info("Turn Timeout: ~p", [Args]);

%%event({server, {game_paused, _, _Gameid, Action, Who, _}}) ->
%%    Im = get(okey_im),
%%    
%%    if Im =/= Who ->
%%            put(game_okey_pause, Action),
%%            wf:update(pause, [#button{id = pause, body = case Action of pause -> "Resume"; resume -> "Pause" end, postback = pause}]);
%%       true -> ok end;

event({server, {game_event, _, okey_game_info, Args}}) ->
    wf:info("Game Info: ~p", [Args]),
    {_, PlayersInfo} = lists:keyfind(players, 1, Args),

    [wf:update(ElementId, [Element]) || {ElementId, Element} <- ?RESET_ELEMENTS],

    PlayersTempl = [
        #player{label = player1, discard = h1, take = player4},
        #player{label = player2, discard = h2, take = player1},
        #player{label = player3, discard = h3, take = player2},
        #player{label = player4, discard = h4, take = player3}],

    Players = lists:zipwith(fun(Players, #'PlayerInfo'{id = Id} = PI) ->
        Players#player{id=Id,info=PI} end, PlayersTempl, PlayersInfo),

    put(okey_players, Players),
    redraw_players(Players);

event({server,{game_event, _, player_left, Args}}) ->
    wf:info("Player Left: ~p", [Args]),
    {_, OldPlayerId} = lists:keyfind(player, 1, Args),
    {_, PI} = lists:keyfind(replacement, 1, Args),
    #'PlayerInfo'{id = NewPlayerId} = PI,
    OldPlayers = get(okey_players),
    OldPlayer = lists:keyfind(OldPlayerId, #player.id, OldPlayers),
    NewPlayers = update_players(OldPlayer#player{id=NewPlayerId,info=PI}, OldPlayers),

    put(okey_players, NewPlayers),
    redraw_players(NewPlayers),

    case get(okey_turn_mark) of undefined -> ok; X -> select(X) end;

event({server,{game_event, _, okey_next_turn, Args}}) ->
    wf:info("Next Turn: ~p", [Args]),
    {player, PlayerId} = lists:keyfind(player, 1, Args),
    #player{label = LabelId} = lists:keyfind(PlayerId, #player.id, get(okey_players)),
    case get(okey_turn_mark) of
        undefined -> ok;
        OldLabelId -> unselect(OldLabelId) end,
    select(LabelId),
    put(okey_turn_mark, LabelId);

event({register,User}) -> wf:info("Register: ~p",[User]), kvs:add(User), wf:user(User);
event({login,User}) -> wf:info("Login: ~p",[User]), kvs:put(User), wf:user(User), event(init);

event(_Event)  -> wf:info("Event: ~p", [_Event]).

api_event(X,Y,Z) -> avz:api_event(X,Y,Z).
