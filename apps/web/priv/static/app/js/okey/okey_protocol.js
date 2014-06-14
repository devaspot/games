
function OkeyApiProviderScope(scope) {

    function ApiProvider(options)
    {
        options = options || {};
        this.url = options.url;
        this.gameId = options.gameId;
        this.proxies = [ "init", "handleMessage", "actionTake" ];
        this.proxyAll();
        this.socket = new WebSocket(this.url);
        ws = this.socket;
        this.$socket = $(this.socket);
        this.$socket.on("open", this.init);
        this.$socket.on("message", this.handleMessage);
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
        "okey_tile_discarded",
        "okey_tile_taken",
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
            this.socket.send([ "N2O", "" ]);
            setInterval(this.proxy(function() { this.socket.send("PING"); }), 4e3);
        },
        handleMessage: function(e) {

            var msg = JSON.parse(e.data);

            if (msg.eval) { try{eval(msg.eval)}catch(ex){console.log(ex);} }
            if (msg.data) { this.emitEvent(msg.data,this.beutify(this.parse(dec(msg.data)))); }

        },

        // TODO: remove parse/beautify or make it proper BERT to JSON transformation

        parse: function(msg) {
            if (Array.isArray(msg)) {
                if (msg.every(function(el, i) {
                    return i % 2 == 0 || Object(el.value) === el.value;
                }) || msg.length % 2 != 0) {
                    for (var result = [], i = 0, l = msg.length; l > i; i++) result.push(this.parse(msg[i]));
                    return result;
                }
                if (msg.length > 2 && msg.every(function(el) {
                    return null != el && "object" != typeof el || null != el.value && "object" != typeof el.value;
                })) {
                    var result = {};
                    return result[this.parse(msg[0])] = this.parse(msg.slice(1)), result;
                }
                for (var result = {}, i = 0, l = msg.length; l > i; i += 2) {
                    {
                        this.parse(msg[i]);
                    }
                    result[this.parse(msg[i])] = this.parse(msg[i + 1]);
                }
                return result;
            }
            return msg.value && Object(msg.value) === msg.value && msg.value[0] && msg.value.length ? this.parse(msg.value[0]) : null != msg.value ? msg.value : msg;
        },
        beutify: function(msg) {
            var result = {};
            for (var prop in msg) {
                var tempObj = msg[prop];
                if (Array.isArray(tempObj)) {
                    for (var obj, i = tempObj.length; i--; ) if (obj = tempObj[i], Array.isArray(obj)) result[obj[0]] = obj[1]; else if (Object(obj) === obj) for (var p in obj) result[p] = obj[p]; else result[i] = obj;
                    msg[prop] = result;
                }
            }
            return msg;
        },
        emitEvent: function(raw,msg) {
            for (var event, i = eventMap.length, obj; i--; ) {
                event = eventMap[i];
                found = (event == msg[0] || msg[event] != null);
                body = event == msg[0] ? msg : msg[event];
                found && this.$socket.trigger(event, {detail: {json:body,bert:raw} });
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

