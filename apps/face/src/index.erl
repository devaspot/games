-module(index).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include("../../server/include/requests.hrl").
-include("../../server/include/settings.hrl").
-jsmacro([take/2,attach/1,join/1,discard/3]).

join(Game) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("join_game"), Game)))).

attach(Token) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("session_attach"), Token)))).

take(GameId,Place) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(bert:atom("game_action"),GameId, bert:atom("okey_take"),[{pile,Place}])))).

discard(GameId, Color, Value) ->
    ws:send(bert:encodebuf(bert:tuple(
        bert:atom('client'),
        bert:tuple(
            bert:atom("game_action"),
            GameId,
            bert:atom("okey_discard"),
            [{tile, bert:tuple(bert:atom("OkeyPiece"), Color, Value)}])))).

redraw_tiles([{Tile, _}| _ ] = TilesList) ->
    wf:update(dddiscard, [#dropdown{id = dddiscard, postback = combo, value = Tile, source = [dddiscard], options = [#option{label = CVBin, value = CVBin} || {CVBin, _} <- TilesList]}]).

main() -> #dtl{file="index", bindings=[{title,<<"N2O">>},{body,body()}]}.

body() ->
    [ #panel{ id=history },

      #label{ id = player1, body = "Player 1", style = "color=black;"}, #label{ id = player2, body = "Player 2", style = "color=black;"},
      #label{ id = player3, body = "Player 3", style = "color=black;"}, #label{ id = player4, body = "Player 4", style = "color=black;"},
      #br{},
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #dropdown{ id=ddtake, value="0", postback=combo, source=[ddtake],
                 options = 
                     [
                      #option { label= <<"0">>, value= <<"0">> },
                      #option { label= <<"1">>, value= <<"1">> }
                     ]
               },
      #button{ id = take, body = <<"Take">>, postback = take, source = [ddtake]},
      #dropdown{ id=dddiscard, value="2", postback=combo, source=[dddiscard], 
                 options = 
                     [
                      #option { label= <<"Option 1">>, value= <<"1">> },
                      #option { label= <<"Option 2">>, value= <<"2">> },
                      #option { label= <<"Option 3">>, value= <<"3">> }
                     ]
               },
      #button{ id = discard, body = <<"Discard">>, postback = discard, source=[dddiscard]}
    ].

event(terminate) -> wf:info("terminate");
event(init) ->
    {ok,GamePid} = game_session:start_link(self()),
%    event(attach),
%    event(join),
    wf:info("INIT ~p",[GamePid]),
    put(game_session, GamePid);

event(combo)  -> wf:info("Combo: ~p",[wf:q(dddiscard)]);
event(join)   -> wf:wire(join("1000001"));
event(attach) -> wf:wire(attach("'"++?TEST_TOKEN++"'"));
event(take)   -> wf:wire(take("1000001", wf:q(ddtake)));

event(discard) -> 
    TilesList = get(game_okey_tiles),
%    wf:info("dd ~p", [wf:q(dddiscard)]),
    {_, {C, V}} = lists:keyfind(erlang:list_to_binary(wf:q(dddiscard)), 1, TilesList),
    wf:wire(discard("1000001", erlang:integer_to_list(C), erlang:integer_to_list(V)));

event({client,Message}) ->
    GamePid = get(game_session),
    game_session:process_request(GamePid, Message);

event({server, {game_event, _, okey_game_started, Args}}) ->
    {_, Tiles} = lists:keyfind(tiles, 1, Args),
    TilesList = [{erlang:list_to_binary([erlang:integer_to_list(C), " ", erlang:integer_to_list(V)]), {C, V}} || {_, C, V} <- Tiles],
    wf:info("tiles ~p", [TilesList]),
    put(game_okey_tiles, TilesList),
    redraw_tiles(TilesList);
event({server, {game_event, _, okey_tile_discarded, Args}}) ->
    Im = get(okey_im),
    {_, Player} = lists:keyfind(player, 1, Args),

    if
       Im == Player ->
            {_, {_, C, V}} = lists:keyfind(tile, 1, Args),
%            wf:info("c ~p v ~p", [C, V]),
            TilesListOld = get(game_okey_tiles),
            TilesList = lists:keydelete({C, V}, 2, TilesListOld),
            put(game_okey_tiles, TilesList),
            redraw_tiles(TilesList);
       true ->
            ok
    end;
event({server, {game_event, _, okey_tile_taken, Args}}) ->
    Im = get(okey_im),
    {_, Player} = lists:keyfind(player, 1, Args),
    if
       Im == Player ->
            case lists:keyfind(revealed, 1, Args) of
                {_, {_, C, V}} ->
                    TilesList = [{erlang:list_to_binary([erlang:integer_to_list(C), " ", erlang:integer_to_list(V)]), {C, V}} | get(game_okey_tiles)],
                    put(game_okey_tiles, TilesList),
                    redraw_tiles(TilesList);
                _ ->
                    ok
            end;
       true ->
            ok
    end;
event({server,{game_event, Game, okey_turn_timeout, Args}}) ->
    wf:info("okey_turn_timeout ~p", [Args]),
    {_, TileTaken} = lists:keyfind(tile_taken, 1, Args),
    event({server, {game_event, Game, okey_tile_taken, [{player, get(okey_im)}, {revealed, TileTaken}]}}),
    {_, TileDiscarded} = lists:keyfind(tile_discarded, 1, Args),
    event({server, {game_event, Game, okey_tile_discarded, [{player, get(okey_im)}, {tile, TileDiscarded}]}});
event({server, {game_event, _, okey_game_info, Args}}) ->
    wf:info("okay_game_info ~p", [Args]),
    {_, PlayersInfo} = lists:keyfind(players, 1, Args),
%    wf:info("pi ~p", [PlayersInfo]),
    Players = 
        lists:zipwith(
          fun(ListId, {PlayerId, PlayerLabel}) ->
                  {ListId, PlayerId, PlayerLabel}
          end,
          [player1, player2, player3, player4],
          lists:map(
            fun
                (#'PlayerInfo'{id = Id, robot = true} = P) ->
%                    wf:info("pp ~p", [P]),
                    {Id, <<Id/binary, <<" R ">>/binary>>};
                (#'PlayerInfo'{id = Id, robot = false} = P) ->
%                    wf:info("pr ~p", [P]),
                    put(okey_im, Id),
                    {Id, <<Id/binary, <<" M ">>/binary>>}
            end,
            PlayersInfo
           )
         ),
    wf:info("players ~p", [Players]),
    put(okey_players, Players),
    [wf:update(LabelId, [#label{id = LabelId, body = PlayerLabel}]) || {LabelId, _, PlayerLabel}  <- Players];
event({server,{game_event, _, okey_next_turn, Args}}) ->
    {player, PlayerId} = lists:keyfind(player, 1, Args),
    {LabelId, _, _} = lists:keyfind(PlayerId, 2, get(okey_players)),
    case get(okey_turn_mark) of
        undefined ->
            ok;
        OldLabelId -> 
            wf:wire("document.querySelector('#" ++ erlang:atom_to_list(OldLabelId) ++ "').style.color = \"black\";")
    end,
    wf:wire("document.querySelector('#" ++ erlang:atom_to_list(LabelId) ++ "').style.color = \"red\";"),
    put(okey_turn_mark, LabelId);
event(Event)  -> ok. % wf:info("Event: ~p", [Event]).
