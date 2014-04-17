-module(index).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("../../server/include/requests.hrl").
-include("../../server/include/settings.hrl").
-include_lib("avz/include/avz.hrl").
-include_lib("kvs/include/user.hrl").

-define(GAMEID, 1000001).

-record(okey_player,
        {
          player_id,
          label_id,
          player_info,
          right_pile_combo_id,
          right_pile = [],
          left_label_id
        }
       ).

user() -> 
    case wf:user() of undefined ->
        Imagionary = fake_users:imagionary_users(),
        {Id,Name,Surname} = lists:nth(crypto:rand_uniform(1,length(Imagionary)),Imagionary),
        X = #user{id = fake_users:fake_id(Id),
                  names = Name,
                  surnames = Surname},
        wf:user(X), X; U-> U end.

color(Id,Color) -> wf:wire(wf:f("document.querySelector('#~s').style.color = \"~s\";",[Id,Color])).
unselect(Id) -> color(Id,black).
select(Id) -> color(Id,red).


redraw_discard_combo(TilesList) ->
    redraw_tiles(TilesList, #dropdown{id = discard_combo, postback = combo, source = [discard_combo]}).

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
      end || #okey_player{label_id = LabelId, player_info =  #'PlayerInfo'{id = Id} = PI} <- Players].

update_players(UpdatedPlayer = #okey_player{label_id = LabelId}, Players) ->
    lists:sort(
      fun(#okey_player{label_id = E1}, #okey_player{label_id = E2}) -> E1 < E2 end,
      [UpdatedPlayer | lists:keydelete(LabelId, #okey_player.label_id, Players)]
     ).

player_name(PI) -> auth_server:player_name(PI).

main() -> #dtl{file="index", bindings=[{title,<<"N2O">>},{body,body()}]}.

body() ->
    wf:wire(#api{name=plusLogin, tag=plus}),
    [ #panel{ id=history },
      #button{ id = plusloginbtn, body = <<"Login">>, postback=login_button},
      #label{ body = " Google"},#br{},#br{},

      #label{ id = gosterge, body="Gosterge: "}, #br{},#br{},

      #label{ id = player1, body = "Player 1"},
      #dropdown{ id = p1right_combo, options = []},#br{},

      #label{ id = player2, body = "Player 2"},
      #dropdown{ id = p2right_combo, options = []},#br{},

      #label{ id = player3, body = "Player 3"},
      #dropdown{ id = p3right_combo, options = []}, #br{},
      
      #label{ id = player4, body = "Player 4"},
      #dropdown{ id = p4right_combo, options = []},#br{},

      #br{},
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #dropdown{ id= take_combo, value="0",
                 options = 
                     [
                      #option { label= <<"Table">>, value= <<"0">> },
                      #option { label= <<"Left">>, value= <<"1">> }
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
      #button{ id = i_saw_okey, body = <<"I Saw Okey">>, postback = i_saw_okey},
      #button{ id = i_have_8_tashes, body = <<"I have 8 Tashes">>, postback = i_have_8_tashes},
      #button{ id = pause, body = <<"Pause">>, postback = pause},
      #button{ id = player_info, body = <<"PlayerInfo">>, postback = player_info}
    ].

event(terminate) -> wf:info("terminate");
event(init) -> event(attach), event(join);
event(login_button) -> wf:wire(protocol:logout());
event(join) -> wf:wire(protocol:join(wf:to_list(?GAMEID)));
event(take) -> wf:wire(protocol:take(wf:to_list(?GAMEID), wf:q(take_combo)));

event(player_info) -> 
    User = user(),
    wf:wire(protocol:player_info(
        wf:f("'~s'",[wf:to_list(User#user.id)]),wf:f("'~s'",[game_okey])));

event(attach) -> 
    {ok,GamePid} = game_session:start_link(self()),
    wf:session(<<"game_pid">>,GamePid),
    User = user(),
    put(okey_im, User#user.id),
    wf:info("Session User: ~p",[User]),
    Token = auth_server:generate_token(?GAMEID,User),
    wf:wire(protocol:attach(wf:f("'~s'",[Token]))),
    ok;

event(discard) -> 
    TilesList = get(game_okey_tiles),
    DiscardCombo = wf:q(discard_combo),
    case lists:keyfind(erlang:list_to_binary(DiscardCombo), 1, TilesList) of
    {_, {C, V}} ->
        wf:wire(protocol:discard(wf:to_list(?GAMEID), wf:to_list(C), wf:to_list(V)));
    false -> wf:info("Discard Combo: ~p",[DiscardCombo]) end;

event(reveal) ->
    TilesList = case get(game_okey_tiles) of undefined -> []; T -> T end,
    Discarded = wf:q(discard_combo),

    case lists:keyfind(wf:to_binary(Discarded), 1, TilesList) of
        {_, {CD, VD} = Key} ->
            Hand = [{C,V} || {_, {C, V}} <- lists:keydelete(Key, 2, TilesList) ],
            HandJS = "[[" ++ string:join([
                wf:f("tuple(atom('OkeyPiece'),~p,~p)",[C,V]) || {C,V} <- Hand],",") ++ "],[]]",
            RevealJS = protocol:reveal(wf:to_list(?GAMEID),wf:f("~p",[CD]),wf:f("~p",[VD]),HandJS),
            wf:info("RevealJS: ~p",[lists:flatten(RevealJS)]),
            wf:wire(RevealJS);
        _ ->
            wf:info("error discarded ~p", Discarded)
    end;

event(i_saw_okey) ->
    wf:info("i_saw_okey!"),
    wf:wire(protocol:i_saw_okey(wf:to_list(?GAMEID)));

event(i_have_8_tashes) ->
    wf:info("i_gave_8_tashes!"),
    wf:wire(protocol:i_have_8_tashes(wf:to_list(?GAMEID)));

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
    wf:wire(protocol:pause(wf:to_list(?GAMEID), wf:f("~p", [Action])));

%event({binary,M}) -> {ok,<<"Hello">>};

event({client,Message}) ->
    case wf:session(<<"game_pid">>) of
        undefined -> skip;
        GamePid -> game_session:process_request(GamePid, Message) end;

event({server, {game_event, _, okey_game_started, Args}}) ->
    {_, Tiles} = lists:keyfind(tiles, 1, Args),
    TilesList = [{wf:to_binary([wf:to_list(C)," ",wf:to_list(V)]), {C, V}} || {_, C, V} <- Tiles],
    %%wf:info("tiles ~p", [TilesList]),
    case lists:keyfind(gosterge, 1, Args) of
        {_, {_, C, V}} ->
            wf:update(gosterge, #label{id = gosterge, body = wf:to_binary(["Gosterge: ", wf:to_list(C), " ", wf:to_list(V)])});
        _ ->
            ok
    end,
    put(game_okey_tiles, TilesList),
    put(game_okey_pause, resume),
    redraw_discard_combo(TilesList);

event({server, {game_event, _, okey_game_player_state, Args}}) ->
    case lists:keyfind(whos_move, 1, Args) of 
        {_, null} ->
            ok;
        {_, WhosMove} ->
            Players = get(okey_players),
            #okey_player{label_id = X} = lists:keyfind(WhosMove, #okey_player.player_id, Players),
            case X of
                null -> skip;
                false -> skip;
                X -> select(X), put(okey_turn_mark,X) end,

            {_, Tiles} = lists:keyfind(tiles, 1, Args),
            TilesList = [{wf:to_binary([wf:to_list(C)," ",wf:to_list(V)]),{C, V}}|| {_, C, V} <- Tiles],
            redraw_discard_combo(TilesList),
            put(game_okey_tiles, TilesList),

            {_, Piles} = lists:keyfind(piles, 1, Args),

            UpdatedPlayers =
                [
                 begin
                     Player = #okey_player{right_pile_combo_id = RightPileComboId} = lists:keyfind(PlayerId, #okey_player.player_id, Players),
                     ConvertedPile = [{wf:to_binary([wf:to_list(C), " ", wf:to_list(V)]), {C, V}} || {_, C, V} <- Pile],
                     redraw_tiles(ConvertedPile, #dropdown{id = RightPileComboId}),
                     Player#okey_player{right_pile = ConvertedPile}
                 end
                 || {PlayerId, Pile} <- Piles
                ],
            put(okey_players, lists:sort(fun(#okey_player{label_id = E1}, #okey_player{label_id = E2}) -> E1 < E2 end, UpdatedPlayers));
        _ -> 
            ok
    end;

event({server, {game_event, _, okey_tile_taken, Args}}) ->
    Im = get(okey_im),

    {_, PlayerId} = lists:keyfind(player, 1, Args),
    case lists:keyfind(revealed, 1, Args) of
        {_, {_, C, V}} ->
            if
                Im == PlayerId ->
                    TilesList = [{wf:to_binary([wf:to_list(C), " ", wf:to_list(V)]), {C, V}} | get(game_okey_tiles)],
                    %%wf:info("Tiles: ~p",[TilesList]),
                    put(game_okey_tiles, TilesList),
                    redraw_discard_combo(TilesList);
                true ->
                    ok
            end,
            case lists:keyfind(pile, 1, Args) of
                {_, 1} -> %% have taken from left
                    Players = get(okey_players),
                    #okey_player{left_label_id = LeftLabelId} = lists:keyfind(PlayerId, #okey_player.player_id, Players),
                    LeftPlayer =
                        #okey_player{right_pile_combo_id = RightPileComboId, right_pile = RightPile} = 
                        lists:keyfind(LeftLabelId, #okey_player.label_id, Players),
                    UpdatedRightPile = lists:keydelete({C, V}, 2, RightPile),
                    redraw_tiles(UpdatedRightPile, #dropdown{id = RightPileComboId}),
                    UpdatedPlayers = update_players(LeftPlayer#okey_player{right_pile = UpdatedRightPile}, Players),
                    put(okey_players, UpdatedPlayers);
                _ ->
                    ok
            end;
        _ -> ok
    end;

event({server, {game_event, _, okey_tile_discarded, Args}}) ->
    Im = get(okey_im),
    {_, PlayerId} = lists:keyfind(player, 1, Args),
    {_, {_, C, V}} = lists:keyfind(tile, 1, Args),

    if
       Im == PlayerId ->
            TilesListOld = get(game_okey_tiles),
            TilesList = lists:keydelete({C, V}, 2, TilesListOld),
            put(game_okey_tiles, TilesList),
            redraw_discard_combo(TilesList);
       true ->
            ok
    end,

    Players = get(okey_players),

    Player = 
        #okey_player{right_pile_combo_id = RightPileComboId, right_pile = OldRightPile} = 
        lists:keyfind(PlayerId, #okey_player.player_id, Players),

    NewRightPile = [{wf:to_binary([wf:to_list(C), " ", wf:to_list(V)]), {C, V}} | OldRightPile],
    redraw_tiles(NewRightPile, #dropdown{id = RightPileComboId}),
    UpdatedPlayer = Player#okey_player{right_pile = NewRightPile},
    UpdatedPlayers = update_players(UpdatedPlayer, Players),
    put(okey_players, UpdatedPlayers);

event({server,{game_event, _Game, okey_turn_timeout, Args}}) ->
    wf:info("okey_turn_timeout ~p", [Args]);

event({server, {game_event, _, okey_game_info, Args}}) ->
%%  wf:info("okay_game_info ~p", [Args]),
    {_, PlayersInfo} = lists:keyfind(players, 1, Args),
    
    PlayersTempl = 
          [
           #okey_player{label_id = player1, right_pile_combo_id = p1right_combo, left_label_id = player4},
           #okey_player{label_id = player2, right_pile_combo_id = p2right_combo, left_label_id = player1},
           #okey_player{label_id = player3, right_pile_combo_id = p3right_combo, left_label_id = player2},
           #okey_player{label_id = player4, right_pile_combo_id = p4right_combo, left_label_id = player3}
          ],
    
    Players = 
        lists:zipwith(
          fun(Players, #'PlayerInfo'{id = Id} = PI) -> 
                  Players#okey_player{player_id = Id, player_info = PI} end,
          PlayersTempl,
          PlayersInfo),

    put(okey_players, Players),

%   wf:info("players ~p", [Players]),

    redraw_players(Players);

event({server,{game_event, _, player_left, Args}}) ->
    {_, OldPlayerId} = lists:keyfind(player, 1, Args),
    {_, PI} = lists:keyfind(replacement, 1, Args),
    #'PlayerInfo'{id = NewPlayerId} = PI,
    OldPlayers = get(okey_players),
    OldPlayer = lists:keyfind(OldPlayerId, #okey_player.player_id, OldPlayers),
    NewPlayers = update_players(OldPlayer#okey_player{player_id = NewPlayerId, player_info = PI}, OldPlayers),

    put(okey_players, NewPlayers),
    redraw_players(NewPlayers),

    case get(okey_turn_mark) of undefined -> ok; X -> select(X) end;

event({server,{game_event, _, okey_next_turn, Args}}) ->
    {player, PlayerId} = lists:keyfind(player, 1, Args),
%    wf:info("im ~p next turn players ~p ~p", [get(okey_im), PlayerId, get(okey_players)]),
    #okey_player{label_id = LabelId} = lists:keyfind(PlayerId, #okey_player.player_id, get(okey_players)),
    case get(okey_turn_mark) of
        undefined -> ok;
        OldLabelId -> unselect(OldLabelId) end,
    select(LabelId),
    put(okey_turn_mark, LabelId);

event({register,User}) -> wf:info("Register: ~p",[User]), kvs:add(User), wf:user(User);
event({login,User}) -> wf:info("Login: ~p",[User]), kvs:put(User), wf:user(User), event(init);

event(_Event)  -> wf:info("Event: ~p", [_Event]).

api_event(X,Y,Z) -> avz:api_event(X,Y,Z).
