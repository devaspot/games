
function StartApp() { $.load(scope.CARD_SOURCE, PostLoad); }

function PostLoad()
{
    rightFlag = 1;
    leftFlag = 1;

    scope.user = document.user;

    $overlay = $("#overlay");

    var centralCard;

    scope.apiProvider = new scope.ApiProvider({url: scope.apiUrl, gameId: scope.gameId });

    createCentralCard();

    var $gosterme = $("#Gosterme");
    scope.ended = !0;

    var playerTurn = !1;

    new scope.Roster(scope);

    function fadeOut()       { $(this).animate({ attributeName: "opacity", from: 1, to: 0, dur: .3}); }
    function fadeIn()        { $(this).animate({ attributeName: "opacity", from: 0, to: 1, dur: .3}); }
    function addFadeOut()    { $(this).on (document.createTouch ? "touchend" : "mouseup", fadeOut); }
    function removeFadeOut() { $(this).off(document.createTouch ? "touchend" : "mouseup", fadeOut); }

    function createCentralCard() {
        scope.centralCard = new scope.Card(),
        scope.centralCard.$el.attr({opacity: 0, transform: "translate(298 -115)" })
            .on(document.createTouch ? "touchstart" : "mousedown", fadeIn)
            .on(document.createTouch ? "touchend"   : "mouseup",   fadeOut);

        scope.centralCard.on("dragstart", scope.deck.select)
            .on("dragmove", removeFadeOut)
            .on("dragstop", addFadeOut)
            .on("dragmove", scope.deck.track)
            .on("revert",   fadeOut);

        scope.centralCard.$el.doubletap(function() { scope.apiProvider.actionTake(scope.centralCard) });
        scope.deck.$el.append(scope.centralCard.$el[0]);
        scope.centralCard.drag();
        scope.centralCard.dragHandler.enable();
    }


    scope.deck.on("take", function(e) {

        e.detail.card.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn)
                         .off(document.createTouch ? "touchend"   : "mouseup",   fadeOut),

        scope.centralCard.off("dragmove", removeFadeOut)
                   .off("dragstop", addFadeOut)
                   .off("revert", fadeOut),

        ~scope.playersLeftHandsMap[scope.user].cards.indexOf(e.detail.card) &&
        scope.playersLeftHandsMap[scope.user].pop(),
        scope.apiProvider.actionTake(e.detail.card);

    });

    scope.apiProvider.on("okey_game_started", initOkeyScene);
    scope.apiProvider.on("okey_game_player_state", initOkeyScene);

    scope.apiProvider.on("okey_game_info", function(x) {


        var e = {detail: x.detail.json, raw: x.detail.bert};
        var playersPositions = scope.playersPositions;
        scope.user = document.user;
        var players = dec(e.raw).value[0][3][0].value[0][1];

        //$overlay.hide(); // give user an ability to see results

        if (!scope.started) {

            scope.playersMap = {};
            scope.playersRightHandsMap = {};
            scope.playersLeftHandsMap = {};

            for (var i = 0; i < players.length; i++) {
                var playerName = players[i].value[0][1].value;
                if (playerName == scope.user) { playersPositions = playersPositions[i]; break; }
            }

            for (var playerInfo, i = 0, l = players.length; l > i; i++) {
                playerInfo = players[i].value[0];
                scope.playersMap[playerInfo[1].value] = scope.playersMap[playerInfo[1].value] || new scope.Player({
                    position: playersPositions[i],
                    sex: playerInfo[12].value,
                    name: [ playerInfo[3].value, playerInfo[4].value ].join(" ")
                });
                var prevPlayer = i == players.length - 1 ? players[0].value[0][1].value : players[i + 1].value[0][1].value;
                for (var prop in scope.playersLeftHandsMap) scope.playersLeftHandsMap[prop].clear();
                scope.playersLeftHandsMap[prevPlayer] = scope.playersRightHandsMap[playerInfo[1].value] = new scope.Hand("#" + [ "Player", playersPositions[i], "Hand" ].join("-")), 
                "Me" == playersPositions[i] && scope.playersRightHandsMap[playerInfo[1].value].$el.droppable({
                    accept: function() {
                        return playerTurn && scope.deck.length() > 14;
                    },
                    drop: function(target) {
                        scope.apiProvider.actionDiscard(target.owner);
                    }
                });


            }

            scope.playersMap[scope.user].timer.on('beforeTimerEnd', function(){ scope.Draggable.revert() })

            scope.started = !0;
        }
    }),

    scope.apiProvider.on("okey_next_turn", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        for (var playerName in scope.playersMap) scope.playersMap[playerName].unselect();

        if (scope.playersMap[e.detail.player].select(), e.detail.player == scope.user)
        {
            playerTurn = !0;
            var cards = scope.playersLeftHandsMap[e.detail.player].cards;
            if (cards.length)
            {
                var card = cards[cards.length - 1];
                scope.deck.$el.append(card.$el[0]), card.$el.attr({
                    transform: "translate(16 -65)"
                }), 
                card.dragHandler.enable(), 
                card
                    .on("dragstart", scope.deck.select)
                    .on("dragmove", scope.deck.track)
                    
                card.$el.doubletap(function(){
                    scope.apiProvider.actionTake(card) 
                });
            }
            scope.deck.length() < 15 ? 
            (   scope.centralCard.dragHandler.enable(),
                scope.centralCard.$el.on(document.createTouch ? "touchstart" : "mousedown", fadeIn).on(document.createTouch ? "touchend" : "mouseup", fadeOut), 
                scope.centralCard.on("dragmove", removeFadeOut).on("dragstop", addFadeOut).on("revert", fadeOut)
            ):( scope.centralCard.dragHandler.disable(), 
                scope.centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut)
            );
        } else {
            playerTurn = !1,
            scope.centralCard.dragHandler.disable(),
            scope.centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut);
        }
    });

    scope.apiProvider.on("okey_tile_discarded", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        if ("object" == typeof e.detail.tile) {
            var c = new scope.Card({
                color: scope.CARD_COLORS[e.detail.tile[1] - 1],
                value: e.detail.tile[2]
            });
            c.log();
        }
        e.detail.player == scope.user && scope.deck.remove(e.detail.tile),
        scope.playersRightHandsMap[e.detail.player].discard(e.detail.tile);
    });

    var $pile = $("#Center-Cards"),
        $fullPile = $pile.find("g").clone(),
        $wholeCards = $("#Stupid-Cards"),
        $fullWholeCards = $("#Stupid-Cards > g").clone();

    scope.apiProvider.on("okey_tile_taken", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        if ("object" == typeof e.detail.revealed) {
            var c = new scope.Card({
                color: scope.CARD_COLORS[e.detail.revealed[1] - 1],
                value: e.detail.revealed[2]
            });
            c.log();
        }

        if (e.detail.pile && !scope.deck.justTaken && scope.playersLeftHandsMap[e.detail.player].take(), 
            0 === e.detail.pile && e.detail.player == scope.user && (scope.centralCard.color = scope.CARD_COLORS[e.detail.revealed[1] - 1], 
            scope.centralCard.value = e.detail.revealed[2], scope.centralCard.render(), createCentralCard()), 
            0 === e.detail.pile)
        {

            var $topCard = $pile.find("g");
            if ($topCard.length > 1) $topCard.last().remove(); else {
                $topCard.last().remove(), $pile.append($fullPile);
                var $miniPile = $wholeCards.find("g");
                $miniPile.length ? $miniPile.first().remove() : $wholeCards.append($fullWholeCards);
            }
        }

        if(e.detail.player == scope.user){
            // scope.Draggable.revert()
            scope.deck.insert(e.detail.revealed)
        }
        scope.centralCard.dragHandler.disable()
        scope.centralCard.$el
            .off(document.createTouch ? 'touchstart' : 'mousedown', fadeIn)
            .off(document.createTouch ? 'touchend' : 'mouseup', fadeOut)

        var cards = scope.playersLeftHandsMap[scope.user].cards;

        if (cards.length) {
            var card = cards[cards.length - 1];
            card.dragHandler.disable();
        }
    });

    scope.apiProvider.on("okey_revealed", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        showRevealHand(dec(e.raw));
//        scope.ended = !0;//, scope.deck.fill([]);
        for (var hand in scope.playersLeftHandsMap) scope.playersLeftHandsMap[hand].clear();
        for (var playerName in scope.playersMap) scope.playersMap[playerName].unselect();
        // $gosterme.remove();
    });

    scope.apiProvider.on("okey_round_ended", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        showRoundEnd(e);
        /*
        var reason = dec(e.raw).value[0][3][1].value[0][1].value;
        var gameres = dec(e.raw).value[0][3][2].value[0][1];
        $("#Overlay-Results").empty();
        for (var i=0;i<gameres.length;i++) { gameresultRow(400,130,i,gameres); }
        if (reason == "tashes_out") {
            $("#Overlay-Text").text("Tashes out");
            $("#RevealDeckRoot").hide();
        }
        */
        scope.ended = !0;
    });

    scope.apiProvider.on("player_left", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var playerInfo = e.detail.replacement.PlayerInfo;
        scope.playersMap[playerInfo[0]] = new scope.Player({
            position: scope.playersMap[e.detail.player].position,
            name: [ playerInfo[2], playerInfo[3] ].join(" "),
            noSkin: !0
        }),

        delete scope.playersMap[e.detail.player], scope.playersRightHandsMap[playerInfo[0]] = scope.playersRightHandsMap[e.detail.player], 
        delete scope.playersRightHandsMap[e.detail.player], scope.playersLeftHandsMap[playerInfo[0]] = scope.playersLeftHandsMap[e.detail.player], 
        delete scope.playersLeftHandsMap[e.detail.player];
    });

    $("#Pause").on("click", function sendPause() { scope.apiProvider.pause(false); });
    $("#Pause").attr({cursor: "pointer"});

    var whoPausedGame = false;

    $overlay.attr({cursor: "pointer"});
    $overlay.on("click", function sendPause() { scope.apiProvider.pause(true); });

    function unpause(e) {
        $overlay.hide();
        for (var player in scope.playersMap) scope.playersMap[player].timer.resume();
    }

    function pause(e) {
        $overlay.show();
        $("#Overlay-Results").empty();
        $("#RevealDeckRoot").hide();
        for (var player in scope.playersMap) scope.playersMap[player].timer.pause();
        var player = scope.playersMap[e.detail[3]];
        $("#Overlay-Text").text(player.name + " paused the game");
    }

    scope.apiProvider.on("game_paused", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        if (whoPausedGame = e.detail[3], "pause" == e.detail[2]) pause(e); else unpause(e);
    });


    $("#Table-Oval").droppable({
        accept: function(target) {
            return 1 === scope.apiProvider.socket.transport().readyState && 
                scope.deck.length() > 14 && 
                target.owner != scope.centralCard && 
                !scope.ended && scope.Card.selected.length <= 1;
        },
        drop: function(target) {
            scope.apiProvider.reveal(target.owner, scope.deck.hand(target.owner));
        }
    });


function initOkeyScene(x)
{

    var e = {detail: x.detail.json, raw: x.detail.bert};

    if (scope.ended = !1, 
        scope.deck.fill(e.detail.tiles),
        scope.deck.render(),
        scope.centralCard.dragHandler.disable(),
        scope.centralCard.$el
            .off(document.createTouch ? "touchstart" : "mousedown", fadeIn)
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
                     hand = scope.playersLeftHandsMap[name],
                     j = playerPile.length; j--; ) hand.discard(playerPile[j]);
    }

    scope.paused = e.detail.paused;

    e.detail.whos_move && "null" != e.detail.whos_move && 
        (e.detail.next_turn_in && "null" != e.detail.next_turn_in && 
         scope.playersMap[e.detail.whos_move].timer.from(e.detail.next_turn_in),
         e.detail.paused && (scope.playersMap[e.detail.whos_move].timer.pause(),$overlay.show()),
         scope.playersMap[e.detail.whos_move].select());
}

function SetupLeftMenu() 
{
    $("#Left-Menu").css("cursor", "pointer").on("click", function() {
        if (leftFlag) { 
            if (currentChat != null)
                $("#OnlineChatEditor")[0].firstElementChild.style.display = "";
            $("#Tournaments").transform({ to: "10 575", from: "44 465" }),
            $("#Promos").transform({ to: "10 575", from: "122 538" }),
            leftFlag = !1
        } else {
            if ($("#OnlineChatEditor")[0].firstElementChild.style.display != "none") 
                $("#OnlineChatEditor")[0].firstElementChild.style.display = "none";
            $("#Tournaments").transform({ from: "10 575", to: "44 465" }),
            $("#Promos").transform({ from: "10 575", to: "122 538" }),
            leftFlag = !0 
        }
    });
}

function SetupRightMenu() 
{
    $("#Right-Menu").css("cursor", "pointer").on("click", function() {

        if (rightFlag) {
            $("#GameChatEditor")[0].firstElementChild.style.display = "";
            $("#Play").transform({to: "975 575", from: "946, 461"});
            $("#Create").transform({to: "975 575",from: "864 526"});
            rightFlag = !1;
        } else {
            if ($("#GameChatEditor")[0].firstElementChild.style.display != "none")
                $("#GameChatEditor")[0].firstElementChild.style.display = "none";
            $("#Play").transform({from: "975 575",to: "946, 461"});
            $("#Create").transform({from: "975 575",to: "864 526"});
            rightFlag = !0;
        }
    });
}

    SetupLeftMenu();
    SetupRightMenu();
    $("#Right-Menu").trigger("click");
    $("#Left-Menu").trigger("click");

}
