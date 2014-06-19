
function OkeyApiProviderScope(scope) {

    function ApiProvider(options)
    {
        options = options || {};
        this.url = options.url;
        this.gameId = options.gameId;
        this.proxies = [ "init", "handleMessage", "actionTake" ];
        this.proxyAll();
        this.socket = new bullet(this.url);
        ws = this.socket;
        this.$socket = $(this.socket.transport());
        this.socket.onopen = this.init;
        this.socket.ondisconnect = this.disconnect;
        this.socket.onmessage = this.handleMessage;
    }

    var eventMap = [
        // general events
        "stats_event",
        // roster protocol
        "online_number",
        "online",
        "chat",
        "offline",
        "roster_item",
        "roster_group",
        "roster_end",
        "chat_message",
        "chat_event",
        // okey game protocol
        "okey_game_info",
        "okey_game_started",
        "okey_game_player_state",
        "okey_next_turn",
        "okey_deny_wrong_reveal",
        "okey_tile_discarded",
        "okey_tile_taken",
        "okey_round_ended",
        "okey_revealed",
        "player_left",
        "game_paused"
    ];

    $.extend(ApiProvider.prototype, {

        proxy: function(func) {
            return func.bind(this);
        },
        proxyAll: function() {
            if (this.proxies) for (var method, i = this.proxies.length; i--; ) method = this.proxies[i], 
            this[method] = this.proxy(this[method]);
        },
        on: function(eventType, handler) {
            this.$socket.on(eventType, handler);
        },
        off: function(eventType, handler) {
            this.$socket.off(eventType, handler);
        },
        init: function() {
            if (!initialized) { this.socket.send([ "N2O", "" ]); initialized = true; }
            console.log("Connected");
            appRun();
        },
        disconnect: function() {
            console.log("Disconnected");
            for (var playerName in scope.playersMap) scope.playersMap[playerName].unselect();
            scope.started = false;
            initialized = false;
        },
        handleMessage: function(e) {
            var msg = JSON.parse(e.data);
            if (msg.eval) { try{eval(msg.eval)}catch(ex){console.log(ex);} }
            if (msg.data) { this.emitEvent(msg.data); }
        },

        emitEvent: function(raw) {
            var msgName = dec(raw).value[0][0].value;
            if (msgName == "game_event") msgName = dec(raw).value[0][2].value;

            for (var event, i = eventMap.length, obj; i--; ) {
                event = eventMap[i];
                if (eventMap[i] == msgName)
                    this.$socket.trigger(msgName, {detail: {json:{},bert:raw} });
            }

        },
        actionTake: function(card) {
            var from = null != card.value ? 1 : 0;
            this.socket.send(enc(tuple(atom("client"), tuple(atom("game_action"), this.gameId, atom("okey_take"), {
                pile: from
            }))));
        },
        actionDiscard: function(card) {
            this.socket.send(enc(tuple(atom("client"), tuple(atom("game_action"), this.gameId, atom("okey_discard"), {
                tile: tuple(atom("OkeyPiece"), scope.CARD_COLORS.indexOf(card.color) + 1, card.value)
            }))));
        },
        reveal: function(card, hand) {
            this.socket.send(enc(tuple(atom("client"), tuple(atom("game_action"), this.gameId, atom("okey_reveal"), {
                discarded: tuple(atom("OkeyPiece"), scope.CARD_COLORS.indexOf(card.color) + 1, card.value),
                hand: hand
            }))));
        },
        pause: function(resume) {
            this.socket.send(enc(tuple(atom("client"),
                tuple(atom("pause_game"),
                    atom("undefined"),
                    this.gameId,
                    atom(resume ? "resume" : "pause")))));
        }
    }),

    scope.ApiProvider = ApiProvider;
}

