
function StartApp()      { $.load(scope.CARD_SOURCE, PostLoad); }

function PostLoad()
{
    rightFlag = 1;
    leftFlag = 1;

    window.deck = scope.deck;
    scope.user = document.user;

    var centralCard,
        apiProvider = new scope.ApiProvider({url: scope.apiUrl, gameId: scope.gameId });

    function fadeOut()       { $(this).animate({ attributeName: "opacity", from: 1, to: 0, dur: .3}); }
    function fadeIn()        { $(this).animate({ attributeName: "opacity", from: 0, to: 1, dur: .3}); }
    function addFadeOut()    { $(this).on (document.createTouch ? "touchend" : "mouseup", fadeOut); }
    function removeFadeOut() { $(this).off(document.createTouch ? "touchend" : "mouseup", fadeOut); }

    function createCentralCard() {
        centralCard = new scope.Card(),
        centralCard.$el.attr({opacity: 0, transform: "translate(298, -115)" })
        .on(document.createTouch ? "touchstart" : "mousedown", fadeIn)
        .on(document.createTouch ? "touchend"   : "mouseup",   fadeOut);

        centralCard.on("dragstart", deck.select).on("dragmove", removeFadeOut)
                                            .on("dragstop", addFadeOut)
                                            .on("dragmove", deck.track)
                                            .on("revert",   fadeOut);

        deck.$el.append(centralCard.$el[0]);
        centralCard.drag();
        centralCard.dragHandler.enable();
    }

    createCentralCard(),

    deck.on("take", function(e) {

        e.detail.card.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn)
                         .off(document.createTouch ? "touchend"   : "mouseup",   fadeOut),

        centralCard.off("dragmove", removeFadeOut)
                   .off("dragstop", addFadeOut)
                   .off("revert", fadeOut),

        ~playersLeftHandsMap[scope.user].cards.indexOf(e.detail.card) &&
        playersLeftHandsMap[scope.user].pop(),

        apiProvider.actionTake(e.detail.card);

    });

    var $gosterme = $("#Gosterme"),
        ended = !0;

    apiProvider.on("okey_game_started", initOkeyScene);
    apiProvider.on("okey_game_player_state", initOkeyScene);

    var playersPositions = 
        [
          [ "Me", "Right", "Center", "Left" ],
          [ "Left", "Me", "Right", "Center" ],
          [ "Center", "Left", "Me", "Right" ],
          [ "Right", "Center", "Left", "Me" ]
         ];

    var playersMap = {},
        playersRightHandsMap = {},
        playersLeftHandsMap = {};

    apiProvider.on("okey_game_info", function(e) {
        scope.user = document.user;
        if (!scope.started) {
            for (var playerInfo, players = e.detail.players, i = 0; i < players.length; i++) 
                if (playerInfo = players[i].PlayerInfo, playerInfo[0] == scope.user)
            {
                playersPositions = playersPositions[i];
                break;
            }
            for (var playerInfo, i = 0, l = players.length; l > i; i++) {
                playerInfo = players[i].PlayerInfo, playersMap[playerInfo[0]] = playersMap[playerInfo[0]] || new scope.Player({
                    position: playersPositions[i],
                    name: [ playerInfo[2], playerInfo[3] ].join(" ")
                });
                var prevPlayer = i == players.length - 1 ? players[0] : players[i + 1];
                for (var prop in playersLeftHandsMap) playersLeftHandsMap[prop].clear();
                playersLeftHandsMap[prevPlayer.PlayerInfo[0]] = playersRightHandsMap[playerInfo[0]] = new scope.Hand("#" + [ "Player", playersPositions[i], "Hand" ].join("-")), 
                "Me" == playersPositions[i] && playersRightHandsMap[playerInfo[0]].$el.droppable({
                    accept: function() {
                        return playerTurn && deck.length() > 14;
                    },
                    drop: function(target) {
                        apiProvider.actionDiscard(target.owner);
                    }
                });
            }


            scope.started = !0;
        }
    }),

    window.playersRightHandsMap = playersRightHandsMap;
    window.playersLeftHandsMap = playersLeftHandsMap;

    var playerTurn = !1;

    apiProvider.on("online_number", function (e) {
//        console.log("Online Number");
    });

    apiProvider.on("okey_next_turn", function(e) {

        for (var playerName in playersMap) playersMap[playerName].unselect();
        if (playersMap[e.detail.player].select(), e.detail.player == scope.user)
        {
            playerTurn = !0;
            var cards = playersLeftHandsMap[e.detail.player].cards;
            if (cards.length)
            {
                var card = cards[cards.length - 1];
                deck.$el.append(card.$el[0]), card.$el.attr({
                    transform: "translate(16 -65)"
                }), card.dragHandler.enable(), card.on("dragstart", deck.select), card.on("dragmove", deck.track);
            }
            deck.length() < 15 ? (centralCard.dragHandler.enable(), centralCard.$el.on(document.createTouch ? "touchstart" : "mousedown", fadeIn).on(document.createTouch ? "touchend" : "mouseup", fadeOut), 
            centralCard.on("dragmove", removeFadeOut).on("dragstop", addFadeOut).on("revert", fadeOut)) : (centralCard.dragHandler.disable(), 
            centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut));
        } else {
            playerTurn = !1,
            centralCard.dragHandler.disable(),
            centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut);
        }
    });

    apiProvider.on("okey_tile_discarded", function(e) {
        if ("object" == typeof e.detail.tile) {
            var c = new scope.Card({
                color: scope.CARD_COLORS[e.detail.tile[1] - 1],
                value: e.detail.tile[2]
            });
            c.log();
        }
        e.detail.player == scope.user && deck.remove(e.detail.tile), playersRightHandsMap[e.detail.player].discard(e.detail.tile);
    });

    var $pile = $("#Center-Cards"),
        $fullPile = $pile.find("g").clone(),
        $wholeCards = $("#Stupid-Cards"),
        $fullWholeCards = $("#Stupid-Cards > g").clone();

    apiProvider.on("okey_tile_taken", function(e) {
        if ("object" == typeof e.detail.revealed) {
            var c = new scope.Card({
                color: scope.CARD_COLORS[e.detail.revealed[1] - 1],
                value: e.detail.revealed[2]
            });
            c.log();
        }

        if (e.detail.pile && !deck.justTaken && playersLeftHandsMap[e.detail.player].take(), 
            0 === e.detail.pile && e.detail.player == scope.user && (centralCard.color = scope.CARD_COLORS[e.detail.revealed[1] - 1], 
            centralCard.value = e.detail.revealed[2], centralCard.render(), createCentralCard()), 
            0 === e.detail.pile)
        {

            var $topCard = $pile.find("g");
            if ($topCard.length > 1) $topCard.last().remove(); else {
                $topCard.last().remove(), $pile.append($fullPile);
                var $miniPile = $wholeCards.find("g");
                $miniPile.length ? $miniPile.first().remove() : $wholeCards.append($fullWholeCards);
            }
        }
        e.detail.player == scope.user && deck.insert(e.detail.revealed), centralCard.dragHandler.disable(), 
        centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn)
                       .off(document.createTouch ? "touchend" : "mouseup", fadeOut);

        var cards = playersLeftHandsMap[scope.user].cards;

        if (cards.length) {
            var card = cards[cards.length - 1];
            card.dragHandler.disable();
        }
    });

    apiProvider.on("okey_revealed", function(e) {
        ended = !0, alert(e.detail.player), deck.fill([]);
        for (var hand in playersLeftHandsMap) playersLeftHandsMap[hand].clear();
        for (var playerName in playersMap) playersMap[playerName].unselect();
        $gosterme.remove();
    });

    apiProvider.on("player_left", function(e) {
        var playerInfo = e.detail.replacement.PlayerInfo;
        playersMap[playerInfo[0]] = new scope.Player({
            position: playersMap[e.detail.player].position,
            name: [ playerInfo[2], playerInfo[3] ].join(" "),
            noSkin: !0
        }),

        delete playersMap[e.detail.player], playersRightHandsMap[playerInfo[0]] = playersRightHandsMap[e.detail.player], 
        delete playersRightHandsMap[e.detail.player], playersLeftHandsMap[playerInfo[0]] = playersLeftHandsMap[e.detail.player], 
        delete playersLeftHandsMap[e.detail.player];
    });

    $("#Pause").on("click", function() { apiProvider.pause(); });

    var whoPausedGame = false;

    //$overlay = $("#overlay");
//    $overlay.on("click", function() { whoPausedGame == scope.user && apiProvider.pause(!0); });
/*
    apiProvider.on("game_paused", function(e) {
        if (whoPausedGame = e.detail[3], "pause" == e.detail[2]) {
            $overlay.show();
            for (var player in playersMap) playersMap[player].timer.pause();
            var player = playersMap[e.detail[3]];
            $overlay.find("text").text(player.name + " paused the game");
        } else {
            $overlay.hide();
            for (var player in playersMap) playersMap[player].timer.resume();
        }
    });
*/
    $("#Table-Oval").droppable({
        accept: function(target) {
            return 1 === apiProvider.socket.readyState && deck.length() > 14 && 
                target.owner != centralCard && !ended && scope.Card.selected.length <= 1;
        },
        drop: function(target) {
            apiProvider.reveal(target.owner, deck.hand(target.owner));
        }
    });

function initOkeyScene(e)
{
    if (ended = !1, 
        scope.deck.fill(e.detail.tiles),
        scope.deck.render(),
        centralCard.dragHandler.disable(),
        centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn)
                       .off(document.createTouch ? "touchend"   : "mouseup",   fadeOut), 
        e.detail.gosterge && "null" != e.detail.gosterge)
    {
        var gosterme = new scope.Card({
            color: scope.CARD_COLORS[e.detail.gosterge[1] - 1],
            value: e.detail.gosterge[2]
        });
        gosterme.$el.attr({transform: "translate(16,-60)"}),
        $gosterme.append(gosterme.$el);
    }

    var piles = e.detail.piles;

    if (piles && "null" != piles) for (var i = 0; i < piles.length; i++)
    {
        var pile = piles[i];
        for (var name in pile) 
            for (var playerPile = pile[name],
                     hand = playersLeftHandsMap[name],
                     j = playerPile.length; j--; ) hand.discard(playerPile[j]);
    }

    e.detail.whos_move && "null" != e.detail.whos_move && 
        (e.detail.next_turn_in && "null" != e.detail.next_turn_in && playersMap[e.detail.whos_move].timer.from(e.detail.next_turn_in),
         e.detail.paused && (playersMap[e.detail.whos_move].timer.pause(),$overlay.show()),
         playersMap[e.detail.whos_move].select());
}

function SetupLeftMenu() 
{
    $("#Left-Menu").css("cursor", "pointer").on("click", function() {
        leftFlag ? (    $("#Tournaments").transform({ to: "10 575", from: "44 465" }),
                        $("#Promos").transform({ to: "10 575", from: "122 538" }),
                        leftFlag = !1
                ) : (   $("#Tournaments").transform({ from: "10 575", to: "44 465" }),
                        $("#Promos").transform({ from: "10 575", to: "122 538" }),
                        leftFlag = !0 );
    });
}

function SetupRightMenu() 
{
    $("#Right-Menu").css("cursor", "pointer").on("click", function() {
        rightFlag ? (   $("#Play").transform({to: "975 575", from: "946, 461"}),
                        $("#Create").transform({to: "975 575",from: "864 526"}),
                        rightFlag = !1
                ) : (   $("#Play").transform({from: "975 575",to: "946, 461"}),
                        $("#Create").transform({from: "975 575",to: "864 526"}),
                        rightFlag = !0 );
    });
}

    SetupLeftMenu();
    SetupRightMenu();

}