
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

    var $pile = $("#Center-Cards"),
        $fullPile = $pile.find("g").clone(),
        $wholeCards = $("#Stupid-Cards"),
        $fullWholeCards = $("#Stupid-Cards > g").clone();

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

    scope.apiProvider.on("okey_game_started", freshGame);
    scope.apiProvider.on("okey_game_player_state", inGameJoined);

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

            for (var playerInfo, i = 0, l = players.length; l > i; i++)
            {
                playerInfo = players[i].value[0];
                scope.playersMap[playerInfo[1].value] = scope.playersMap[playerInfo[1].value] || new scope.Player({
                    position: playersPositions[i],
                    sex: playerInfo[12].value,
                    name: [ playerInfo[3].value, playerInfo[4].value ].join(" ")
                });
                var prevPlayer = i == players.length - 1 ? players[0].value[0][1].value : players[i + 1].value[0][1].value;
                for (var prop in scope.playersLeftHandsMap) scope.playersLeftHandsMap[prop].clear();
                scope.playersLeftHandsMap[prevPlayer] = scope.playersRightHandsMap[playerInfo[1].value] = new scope.Hand("#" + [ "Player", playersPositions[i], "Hand" ].join("-")), 

                "Me" == playersPositions[i] && 
                scope.playersRightHandsMap[playerInfo[1].value].$el.droppable({
                    accept: function() { return playerTurn && scope.deck.length() > 14; },
                    drop: function(target) { scope.apiProvider.actionDiscard(target.owner); }
                });
            }

            scope.playersMap[scope.user].timer.on('beforeTimerEnd', function(){ scope.Draggable.revert() });
            scope.started = !0;
        }
    }),

    scope.apiProvider.on("okey_next_turn", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var player = dec(e.raw).value[0][3][0].value[0][1].value;
         var enabled = dec(e.raw).value[0][3][2].value[0][1].value;
        for (var playerName in scope.playersMap) scope.playersMap[playerName].unselect();

        updateOkeyButton(player, enabled);

        if (scope.playersMap[player].select(), player == scope.user)
        {
            playerTurn = !0;
            var cards = scope.playersLeftHandsMap[player].cards;
            if (cards.length)
            {
                var card = cards[cards.length-1];
                scope.deck.$el.append(card.$el[0]);
                card.$el.attr({ transform: "translate(16 -65)"});
                card.dragHandler.enable();
                card.on("dragstart", scope.deck.select)
                    .on("dragmove", scope.deck.track);
                card.$el.doubletap(function(){ scope.apiProvider.actionTake(card) });
            }
            scope.deck.length() < 15 ? 
            (   scope.centralCard.dragHandler.enable(),
                scope.centralCard.$el.on(document.createTouch ? "touchstart" : "mousedown", fadeIn).on(document.createTouch ? "touchend" : "mouseup", fadeOut), 
                scope.centralCard.on("dragmove", removeFadeOut).on("dragstop", addFadeOut).on("revert", fadeOut),
                $("#Gabrielo-Discard-Shape").show(),
                $("#Center-Card-Selection").show()

            ):( $("#You-Discard-Shape").show(),
                scope.centralCard.dragHandler.disable(), 
                scope.centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut)
            );

        } else {

            $("#You-Discard-Shape").hide();

            playerTurn = !1,
            scope.centralCard.dragHandler.disable(),
            scope.centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut);
        }
    });

    scope.apiProvider.on("okey_tile_discarded", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var player = dec(e.raw).value[0][3][0].value[0][1].value;
        var tile = dec(e.raw).value[0][3][1].value[0][1].value[0];
        if (null != tile.length)
            new scope.Card({color:scope.CARD_COLORS[tile[1]-1],value:tile[2]});
        player == scope.user && scope.deck.remove(tile),
        scope.playersRightHandsMap[player].discard(tile);
    });


    scope.apiProvider.on("okey_tile_taken", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};

        var player = dec(e.raw).value[0][3][0].value[0][1].value;
        var pile = dec(e.raw).value[0][3][1].value[0][1];
        var revealed = dec(e.raw).value[0][3][2].value[0][1].value;
        var pile_height = dec(e.raw).value[0][3][3].value[0][1];

        if (0 == pile)
            $("#Pile-Height")[0].lastElementChild.textContent = 
                pile_height == 0 ? "" : pile_height;

        if (revealed != "null") {
            revealed = revealed[0];
            new scope.Card({color:scope.CARD_COLORS[revealed[1]-1],value:revealed[2]});
        }

        if (player == scope.user) {
            $("#Gabrielo-Discard-Shape").hide();
            $("#Center-Card-Selection").hide();
            $("#You-Discard-Shape").show();
        }

        if (pile && !scope.deck.justTaken && scope.playersLeftHandsMap[player].take(), 
            0 === pile && player == scope.user && (scope.centralCard.color = scope.CARD_COLORS[revealed[1]-1], 
            scope.centralCard.value = revealed[2], scope.centralCard.render(), createCentralCard()), 
            0 === pile)
        {

            var $topCard = $pile.find("g");
            if ($topCard.length > 1) $topCard.last().remove();
            else { 
                $topCard.last().remove();
                if (pile_height > 0) $pile.append($fullPile); 
                var $miniPile = $wholeCards.find("g");
//                $miniPile.length ? $miniPile.first().remove() : $wholeCards.append($fullWholeCards);
                $miniPile.first().remove();
            }

        }

        if(player == scope.user){
            // scope.Draggable.revert()
            scope.deck.insert(revealed)
        }

        scope.centralCard.dragHandler.disable();

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
        // $gosterme.remove();
    });

    scope.apiProvider.on("wrong_reveal", function(x) {
        denyWrongReveal();
    });

    scope.apiProvider.on("okey_enable", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var player = dec(e.raw).value[0][3][0].value[0][1].value;
        var enabled = dec(e.raw).value[0][3][1].value[0][1].value;
        updateOkeyButton(player, enabled);
    });

    scope.apiProvider.on("okey_round_ended", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        showRoundEnd(e);
        for (var hand in scope.playersLeftHandsMap) scope.playersLeftHandsMap[hand].clear();
        for (var playerName in scope.playersMap) scope.playersMap[playerName].unselect();
        scope.ended = !0;
    });

    scope.apiProvider.on("player_left", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var player = dec(e.raw).value[0][3][0].value[0][1].value;
        var playerInfo = dec(e.raw).value[0][3][2].value[0][1].value[0];
        scope.playersMap[playerInfo[1].value] = new scope.Player({
            position: scope.playersMap[player].position,
            name: [ playerInfo[3].value, playerInfo[4].value ].join(" "),
            noSkin: !0
        }),

        delete scope.playersMap[player];
        scope.playersRightHandsMap[playerInfo[1].value] = scope.playersRightHandsMap[player];
        delete scope.playersRightHandsMap[player];
        scope.playersLeftHandsMap[playerInfo[1].value] = scope.playersLeftHandsMap[player];
        delete scope.playersLeftHandsMap[player];

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
        var player = scope.playersMap[e.who];
        $("#Overlay-Text").text(player.name + " paused the game");
    }

    scope.apiProvider.on("game_paused", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var who = dec(e.raw).value[0][4].value;
        var what = dec(e.raw).value[0][3].value;
        whoPausedGame = who;
        if ("pause" == what) pause({who:who}); else unpause({});
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

function updateOkeyButton(player, enabled)
{
        if (enabled) {
            // $("#Okey").show();
            $("#Okey").find("rect").attr("fill","red");
        } else {
            // $("#Okey").hide();
            $("#Okey").find("rect").attr("fill","#517ECE");
            $("#Gabrielo-Discard-Shape").hide();
            var cards = scope.playersLeftHandsMap[scope.user].cards;
            if (cards.length) cards[cards.length-1].dragHandler.disable();
        }
}

function freshGame(x)
{
    var e = {detail: x.detail.json, raw: x.detail.bert};
    initOkeyScene({
        tiles: dec(e.raw).value[0][3][0].value[0][1],
        gosterme: dec(e.raw).value[0][3][1].value[0][1].value,
        pile_height: dec(e.raw).value[0][3][2].value[0][1],
        cur_round: dec(e.raw).value[0][3][3].value[0][1]
    });


}

function reminder(pile_height)
{
    var pile_rem = pile_height % 5;
    for (var i=0;i<9-Math.floor(pile_height/5);i++) $wholeCards.find("g").first().remove();
    if (pile_rem > 0) for (var i=0;i<5-pile_rem;i++) $pile.find("g").last().remove();
}

function inGameJoined(x)
{
    var e = {detail: x.detail.json, raw: x.detail.bert};
    initOkeyScene({
        whos_move: dec(e.raw).value[0][3][0].value[0][1].value,
        piles: dec(e.raw).value[0][3][2].value[0][1],
        tiles: dec(e.raw).value[0][3][3].value[0][1],
        gosterme: dec(e.raw).value[0][3][4].value[0][1].value,
        pile_height: dec(e.raw).value[0][3][5].value[0][1],
        cur_round: dec(e.raw).value[0][3][6].value[0][1],
        next_turn_in: dec(e.raw).value[0][3][8].value[0][1],
        paused: dec(e.raw).value[0][3][9].value[0][1].value
    });

    new Audio("mp3/ding.mp3").play();

}

function initOkeyScene(x)
{
    if (x.gosterme && "null" != x.gosterme) x.gosterme = x.gosterme[0];

    $pile.empty();
    $pile.append($fullPile);
    $wholeCards.empty();
    $wholeCards.append($fullWholeCards);

    $("#Okey").find("rect").attr("fill","#517ECE");

    if (null != x.pile_height && "null" != x.pile_height) {
        $("#Pile-Height")[0].lastElementChild.textContent = x.pile_height;
        reminder(x.pile_height);
    }

    if (scope.ended = !1, 
        scope.deck.fill(x.tiles),
        scope.deck.render(),
        scope.centralCard.dragHandler.disable(),
        scope.centralCard.$el
            .off(document.createTouch ? "touchstart" : "mousedown", fadeIn)
            .off(document.createTouch ? "touchend"   : "mouseup",   fadeOut), 
        x.gosterme && "null" != x.gosterme)
    {
        var gosterme = new scope.Card({color:scope.CARD_COLORS[x.gosterme[1]-1],value:x.gosterme[2]});
        gosterme.$el.attr({transform: "translate(16,-60)"}),
        $gosterme.append(gosterme.$el);
    }

    if (null != x.piles && null != x.piles.length) for (var i = 0; i < x.piles.length; i++)
    {
        var pile = x.piles[i];
        var name = pile.value[0][0].value;
        var playerPiles = pile.value[0][1];
        var hand = scope.playersLeftHandsMap[name];
        for (var j=0;j<playerPiles.length;j++)
        {
            hand.discard(playerPiles[j].value[0]);
        }
    }

    if (null != x.whos_move    && "null" != x.whos_move &&
        null != x.next_turn_in && "null" != x.next_turn_in)
    {
         scope.playersMap[x.whos_move].timer.from(x.next_turn_in);
         if (x.paused) { scope.playersMap[x.whos_move].timer.pause(); $overlay.show() } else unpause();
         scope.playersMap[x.whos_move].select();
    };

    scope.paused = x.paused;

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
