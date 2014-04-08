-module(index).
-compile({parse_transform, shen}).
-compile(export_all).
-include_lib("n2o/include/wf.hrl").
-include_lib("server/include/requests.hrl").
-include_lib("server/include/settings.hrl").
-jsmacro([take/2,attach/1,join/1]).

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
        bert:tuple(bert:atom("game_action"),GameId,bert:atom("okey_take"),[{pile,Place}])))).

%%discard(GameId, Value, Color) ->
%%    ws:send(
%%      bert:encodebuf(
%%        bert:tuple(
%%          bert:atom('client'),
%%          bert:tuple(
%%            bert:atom("game_action"),GameId,bert:atom("okey_discard"),
%%            [bert:atom()]
%%            [{pile,Place}]
%%           )
%%         )
%%       )
%%     ).

redraw_tiles(TilesList) ->
    wf:replace(drop, #dropdown{id = drop, options = [#option{label = VCBin} || {VCBin, _} <- TilesList]}).

main() -> #dtl{file="index", bindings=[{title,<<"N2O">>},{body,body()}]}.

body() ->
    [ #panel{ id=history },
      #dropdown { id=drop, value="2", postback=combo, source=[drop], options=[
        #option { label= <<"Option 1">>, value= <<"1">> },
        #option { label= <<"Option 2">>, value= <<"2">> },
        #option { label= <<"Option 3">>, value= <<"3">> }
     ]},
      #button{ id = attach, body = <<"Attach">>, postback = attach},
      #button{ id = join, body = <<"Join">>, postback = join},
      #button{ id = take, body = <<"Take">>, postback = take},
      #button{ id = discard, body = <<"Discard">>, postback = discard}
    ].

event(init) ->
    {ok,GamePid} = game_session:start_link(self()),
    put(game_session,GamePid);

event(combo)  -> wf:info("Combo: ~p",[wf:q(drop)]);
event(join)   -> wf:wire(join("1000001"));
event(attach) -> wf:wire(attach("'"++?TEST_TOKEN++"'"));
event(take)   -> wf:wire(take("1000001","0"));
event({game_event, _, okey_game_started, Args}) ->
    {_, Tiles} = lists:keyfind(tiles, 1, Args),
    TilesList = [{erlang:list_to_binary([erlang:integer_to_list(V), " ", erlang:integer_to_list(C)]), {V, C}} || {_, V, C} <- Tiles],
    put(game_okey_tiles, TilesList),
    redraw_tiles(TilesList),
    wf:info("tiles ~p", [Tiles]);
event({game_event, _, okey_tile_discarded, Args}) ->
    {_, {_, V, C}} = lists:keyfind(tile, 1, Args),
    TilesListOld = get(game_okey_tiles),
    TilesList = lists:keydelete()
    ;
event(Event)  -> wf:info("Event: ~p", [Event]).
