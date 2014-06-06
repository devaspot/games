var $ = function(_undefind) {
    function Selector(elements) {
        this.length = elements.length;
        for (var i = 0, l = this.length; l > i; i++) this[i] = elements[i];
    }
    function $(selector) {
        return tag.test(selector) ? new Selector([ document.createElementNS("http://www.w3.org/2000/svg", tag.exec(selector)[1]) ]) : "string" == typeof selector ? new Selector(document.querySelectorAll(selector)) : selector instanceof NodeList ? new Selector(selector) : selector instanceof Element ? new Selector([ selector ]) : selector.addEventListener ? new Selector([ selector ]) : selector instanceof Selector ? selector : new Selector([]);
    }
    var fn = Selector.prototype;
    fn.each = function(callback) {
        for (var i = 0, l = this.length; l > i; i++) callback(this[i], i);
        return this;
    }, fn.on = function(eventName, eventHandler) {
        return this.each(function(el) {
            el.addEventListener(eventName, eventHandler);
        });
    }, fn.off = function(eventName, eventHandler) {
        return this.each(function(el) {
            el.removeEventListener(eventName, eventHandler);
        });
    }, fn.trigger = function(eventName, data) {
        return this.each(function(el) {
            event = new CustomEvent(eventName, data), el.dispatchEvent(event);
        });
    }, fn.css = function(name, value) {
        if (Object(name) === name) {
            for (var prop in name) this.css(prop, name[prop]);
            return this;
        }
        return null != value ? this.each(function(el) {
            el.style[name] = value;
        }) : this.length ? getComputedStyle(this[0]).getPropertyValue(name) : _undefind;
    }, fn.show = function() {
        return this.css("display", "block");
    }, fn.hide = function() {
        return this.css("display", "none");
    }, fn.text = function(text) {
        return null != text ? this.each(function(el) {
            el.textContent = text;
        }) : this.length ? this[0].textContent : _undefind;
    }, fn.html = function(html) {
        return null != html ? this.each(function(el) {
            for (;el.firstChild; ) el.removeChild(el.firstChild);
            var fragment = document.createElement("div");
            fragment.innerHTML = "<svg>" + html + "</svg>";
            for (var svg = fragment.firstChild, node = svg.firstChild; node; ) el.appendChild(node.cloneNode(!0)), 
            node = node.nextSibling;
        }) : this.length ? this[0].innerHTML : _undefind;
    }, fn.remove = function() {
        return this.each(function(el) {
            el.parentNode && el.parentNode.removeChild(el);
        });
    }, fn.attr = function(name, value) {
        if (Object(name) === name) {
            for (var prop in name) this.attr(prop, name[prop]);
            return this;
        }
        return null != value ? this.each(function(el) {
            el.setAttribute(name, value);
        }) : this.length ? this[0].getAttribute(name) : _undefind;
    }, fn.removeAttr = function(name) {
        return this.each(function(el) {
            el.removeAttribute(name);
        });
    }, fn.append = function(target) {
        return this.each(function(el) {
            target instanceof Selector ? target.each(function(child) {
                el.appendChild(child);
            }) : el.appendChild(target);
        });
    }, fn.empty = function() {
        return this.each(function(el) {
            for (;el.firstChild; ) el.removeChild(el.firstChild);
        });
    }, fn.eq = function(idx) {
        return new Selector(idx >= this.length ? [] : [ this[idx] ]);
    }, fn.find = function(selector) {
        var result = [];
        return this.each(function(el) {
            Array.prototype.push.apply(result, el.querySelectorAll(selector));
        }), new Selector(result);
    }, fn.parent = function() {
        var result = [];
        return this.each(function(el) {
            result.push(el.parentNode);
        }), new Selector(result);
    }, fn.first = function() {
        return new Selector(this.length ? [ this[0] ] : []);
    }, fn.last = function() {
        return new Selector(this.length ? [ this[this.length - 1] ] : []);
    }, fn.clone = function() {
        var result = [];
        return this.each(function(el) {
            result.push(el.cloneNode(!0));
        }), new Selector(result);
    }, fn.width = function() {
        return this.length ? this[0].getBoundingClientRect().width : _undefind;
    }, fn.height = function() {
        return this.length ? this[0].getBoundingClientRect().height : _undefind;
    }, fn.position = function() {
        if (this.length) {
            var clientRect = this[0].getBoundingClientRect();
            return {
                top: clientRect.top,
                right: clientRect.right,
                bottom: clientRect.bottom,
                left: clientRect.left
            };
        }
        return _undefind;
    };
    var defaultAnim = {
        "class": "anim",
        begin: "indefinite"
    }, anim = function(anim) {
        return $("<animate/>").attr(anim);
    };
    fn.animate = function(anims) {
        var callbacks = [], thenable = {
            then: function(complete) {
                return callbacks.push(complete), thenable;
            }
        };
        return anims = $.extend({}, defaultAnim, anims), this.each(function(el) {
            var $el = $(el), $anim = $el.find(".anim");
            $anim.length ? $anim.attr(anims) : ($el.append(anim(anims)[0]), $anim = $el.find(".anim")), 
            el.timerId = setTimeout(function() {
                $el.attr(anims.attributeName, anims.to), $el.removeAttr("animated"), callbacks.forEach(function(c) {
                    c();
                }), callbacks = [];
            }, 1e3 * parseFloat(anims.dur)), $el.attr("animated", !0), $anim[0].beginElement();
        }), thenable;
    };
    var animDelay = 62.5;
    fn.move = function(anims) {
        return this.each(function(el) {
            function tick() {
                if (!el.paused) {
                    var cur = from + step;
                    (to > from ? to >= cur : cur >= to) ? ($el.attr(property, cur), from = cur) : clearInterval(timer);
                }
            }
            var property = anims.attributeName, dur = anims.dur, to = parseInt(anims.to), $el = $(el), from = parseInt($el.attr(property)), step = (to - from) / (1e3 * dur / animDelay);
            el.timer = setInterval(tick, animDelay);
        });
    };
    var defaultTrf = {
        "class": "trf",
        type: "translate",
        dur: "0.3",
        calcMode: "spline",
        keySplines: "0.215 0.61 0.355 1",
        additive: "replace",
        attributeName: "transform",
        begin: "indefinite",
        keyTimes: "0; 1"
    }, trf = function(trf) {
        return $("<animateTransform/>").attr(trf);
    };
    fn.transform = function(trfs) {
        var callbacks = [], thenable = {
            then: function(complete) {
                return callbacks.push(complete), thenable;
            }
        };
        return trfs = $.extend({}, defaultTrf, trfs), this.each(function(el) {
            var $el = $(el), $anim = $el.find(".trf");
            $anim.length ? $anim.attr(trfs) : ($el.append(trf(trfs)[0]), $anim = $el.find(".trf")), 
            el.timerId = setTimeout(function() {
                $el.attr("transform", trfs.type + "(" + trfs.to + ")"), $el.removeAttr("animated"), 
                callbacks.forEach(function(c) {
                    c();
                }), callbacks = [];
            }, 1e3 * parseFloat(trfs.dur) - 20), $el.attr("animated", !0), $anim[0].beginElement();
        }), thenable;
    }, fn.stop = function() {
        return this.each(function(el) {
            $(el).find(".anim, .trf").each(function(anim) {
                anim.endElement();
            }), clearTimeout(el.timerId), clearInterval(el.timer);
        });
    }, fn.pause = function() {
        return $("svg")[0].pauseAnimations(), this.each(function(el) {
            el.paused = !0;
        });
    }, fn.resume = function() {
        return $("svg")[0].unpauseAnimations(), this.each(function(el) {
            el.paused = !1;
        });
    };
    var tag = /^<(.+)\/>$/;
    return $.extend = function(target) {
        for (var obj, properties, i = 1, l = arguments.length; l > i; i++) {
            obj = arguments[i], properties = Object.keys(obj);
            for (var property, j = properties.length; j--; ) property = properties[j], target[property] = obj[property];
        }
        return target;
    }, $.inherit = function(child, parent) {
        function ctor() {
            this.constructor = child, this.__super__ = parent.prototype;
        }
        return ctor.prototype = parent.prototype, child.prototype = new ctor(), child;
    }, $.mixin = function(plagin) {
        $.extend(fn, plagin);
    }, $.timestamp = 1400668550599, $.load = function(url, complete) {
        url = url + "?q=" + $.timestamp;
        var result = localStorage.getItem(url);
        if (null == result) {
            var xhr = new XMLHttpRequest();
            xhr.open("GET", url, !0), xhr.onload = function() {
                localStorage.setItem(url, xhr.responseText), complete(xhr.responseText);
            }, xhr.send();
        } else complete(result);
    }, $.rand = function(min, max) {
        return min + Math.floor(Math.random() * (max - min + 1));
    }, $;
}(), 

Core = function() {
    var scope = {
        apiUrl: "ws://localhost:8080/ws/",
        gameId: 1000001,
        defaultSessionId: "7hc6Vl0CpxvngjiQQWDspxUtKmhF1zCbT155+M73XhFcKw9e6jkNWLSLM8up4ZQbSUGJXKOphb0YNu4yDRKZkbkiyY1yZukqXFj4nAfvSUNuWx0352VWfLlKZWldHb1na7d7sA==",
        CARD_SOURCE: "svg/card.svg",
        CARD_SMALL_SOURCE: "svg/card-small.svg",
        CARD_COLORS: [ "#CE290F", "#3B5998", "#48AF5E", "#F8E81C" ],
        SKIN_NAMES: [ "alina", "gabrielo", "mustafa" ]
    };
    return function(module) {
        module(scope);
    };
}();

Core(function(scope) {
    function Controller() {
        this.proxyAll(), this.refreshElements();
    }
    var isIE = window.navigator.msPointerEnabled;
    $.extend(Controller.prototype, {
        proxy: function(func) {
            return func.bind(this);
        },
        proxyAll: function() {
            if (this.proxies) for (var method, i = this.proxies.length; i--; ) method = this.proxies[i], 
            this[method] = this.proxy(this[method]);
        },
        withDelay: function(func, timeout) {
            return setTimeout(this.proxy(func), timeout || 0);
        },
        $: function(selector) {
            return this.$el.find(selector);
        },
        refreshElements: function() {
            if (this.elements) for (var element in this.elements) this[element] = this.$(this.elements[element]);
        },
        on: function(eventType, handler) {
            return this.$el.on(eventType, handler), this;
        },
        off: function(eventType, handler) {
            return this.$el.off(eventType, handler), this;
        },
        clientX: function(e) {
            return isIE ? e.pageX : document.createTouch ? e.changedTouches[0].clientX : e.clientX;
        },
        clientY: function(e) {
            return isIE ? e.pageY : document.createTouch ? e.changedTouches[0].clientY : e.clientY;
        },
        intersect: function(x, y) {
            var pos = this.$el.position();
            return pos.top < y && pos.bottom > y && pos.left < x && pos.right > x;
        }
    }), scope.Controller = Controller;
}), 

Core(function(scope) {
    function ApiProvider(options) {
        options = options || {}, this.url = options.url, this.sessionId = options.sessionId, 
        this.gameId = options.gameId, this.proxies = [ "init", "handleMessage", "actionTake" ], 
        this.proxyAll(), this.socket = new WebSocket(this.url), this.$socket = $(this.socket), 
        this.$socket.on("open", this.init), this.$socket.on("message", this.handleMessage);
    }
    var eventMap = [ "okey_game_info", "okey_game_started", "okey_game_player_state", "okey_next_turn", "okey_tile_discarded", "okey_tile_taken", "okey_revealed", "player_left", "game_paused" ], 
    session = /\('session_attach'\),'([^)]+)'\)/, 
    userName = /document.user\s?=\s?'([^']+)'/;
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
            this.socket.send([ "N2O", "" ]), setInterval(this.proxy(function() {
                this.socket.send("PING");
            }), 4e3);
        },
        handleMessage: function(e) {
            var msg = JSON.parse(e.data);
            if (msg.eval) {
                var curSession, user;
                (curSession = session.exec(msg.eval)) && (this.sessionId = curSession[1]), (user = userName.exec(msg.eval)) && (scope.user = user[1]);
            }
            msg.data ? (msg = dec(msg.data), this.emitEvent(this.beutify(this.parse(msg)))) : msg.eval && (this.socket.send(enc(tuple(atom("client"), tuple(atom("session_attach"), this.sessionId)))), 
            this.socket.send(enc(tuple(atom("client"), tuple(atom("join_game"), this.gameId)))));
        },
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
        emitEvent: function(msg) {
            for (var event, i = eventMap.length; i--; ) event = eventMap[i], msg[event] && this.$socket.trigger(event, {
                detail: msg[event]
            });
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
            this.socket.send(enc(tuple(atom("client"), tuple(atom("pause_game"), atom("undefined"), this.gameId, atom(resume ? "resume" : "pause")))));
        }
    }), scope.ApiProvider = ApiProvider;
}), 

Core(function(scope) {
    function culcShift() {
        sizeX = $svg[0].viewBox.baseVal.width / innerWidth, sizeY = $svg[0].viewBox.baseVal.height / innerHeight, 
        size = Math.max(sizeX, sizeY) || 1;
    }
    function Draggable(root, options) {
        options = options || {}, this.$el = $(root), this.revert = options.revert, this.elements = {}, 
        this.proxies = [ "onDown", "onMove", "onUp" ], this.__super__.constructor.call(this), 
        this.enable();
    }
    var sizeX, sizeY, size, moved, $svg = $("svg");
    culcShift(), $(window).on("resize", culcShift), $(window).on("orientationchange", culcShift), 
    $.inherit(Draggable, scope.Controller), $.extend(Draggable.prototype, {
        storeTrf: function() {
            var trf = this.$el.attr("transform");
            this.initTrf = trf ? trf.slice(10, -1).split(/\s+/) : [ 0, 0 ];
        },
        disable: function() {
            this.$el.off(document.createTouch ? "touchstart" : "mousedown", this.onDown), this.$el.css({
                cursor: "default"
            });
        },
        enable: function() {
            this.$el.on(document.createTouch ? "touchstart" : "mousedown", this.onDown), this.$el.attr("style", "cursor: -moz-grab; cursor: -webkit-grab; cursor: grab;");
        },
        onDown: function(e, silent) {
            e.preventDefault(), moved || (moved = !1, this.$el.attr("style", "cursor: -moz-grabbing; cursor: -webkit-grabbing; cursor: grabbing;"), 
            this.$el[0].parentNode.appendChild(this.$el[0]), this.x = this.clientX(e), this.y = this.clientY(e), 
            this.trf = this.$el.attr("transform"), this.trf && (this.trf = this.trf.slice(10, -1).split(/\s+/)), 
            this.storeTrf(), silent || (document.createTouch ? (this.$el.on("touchmove", this.onMove), 
            this.$el.on("touchend", this.onUp)) : ($("body").on("mousemove", this.onMove), $("body").on("mouseup", this.onUp)), 
            this.$el.trigger("dragstart")));
        },
        onMove: function(e, silent) {
            e.preventDefault(), moved = !0, this.dx = ((this.curX = this.clientX(e)) - this.x) * size, 
            this.dy = ((this.curY = this.clientY(e)) - this.y) * size, this.trf && (this.dx += 0 | this.trf[0], 
            this.dy += 0 | this.trf[1]), this.$el.attr("transform", "translate(" + this.dx + " " + this.dy + ")"), 
            silent || this.$el.trigger("dragmove", {
                detail: {
                    x: this.curX,
                    y: this.curY,
                    event: e
                }
            });
        },
        onUp: function(e, silent) {
            if (e.preventDefault(), this.$el.attr("style", "cursor: -moz-grab; cursor: -webkit-grab; cursor: grab;"), 
            moved) {
                for (var droped, item, dropList = scope.Droppable.list, i = 0, l = dropList.length; l > i; i++) if (item = dropList[i], 
                item.intersect(this.curX, this.curY)) {
                    droped = item.drop(this, this.curX, this.curY);
                    break;
                }
                droped || (this.$el.transform({
                    from: [ this.dx, this.dy ].join(" "),
                    to: this.initTrf.join(" ")
                }), silent || this.$el.trigger("revert")), silent || this.$el.trigger("dragstop", {
                    detail: {
                        x: this.curX,
                        y: this.curY,
                        event: e
                    }
                }), moved = !1;
            }
            document.createTouch ? (this.$el.off("touchmove", this.onMove), this.$el.off("touchend", this.onUp)) : ($("body").off("mousemove", this.onMove), 
            $("body").off("mouseup", this.onUp));
        }
    }), 
	scope.Draggable = Draggable, 
	$.mixin({
        draggable: function() {
            return this.each(function(el) {
                new Draggable(el);
            });
        }
    });
}), 

Core(function(scope) {
    function Droppable(root, options) {
        options = options || {}, this.$el = $(root), this.accept = options.accept || function() {
            return !0;
        }, this.onDrop = options.drop || function() {}, this.elements = {}, this.proxies = [], 
        this.__super__.constructor.call(this), this.activate();
    }
    Droppable.list = [], $.inherit(Droppable, scope.Controller), $.extend(Droppable.prototype, {
        drop: function(target, x, y) {
            return this.accept(target, x, y) ? (this.onDrop(target, x, y), !0) : !1;
        },
        activate: function() {
            Droppable.list.push(this);
        },
        release: function() {
            var i;
            ~(i = Droppable.list.indexOf(this)) && Droppable.list.splice(i, 1);
        }
    }), 
    scope.Droppable = Droppable, 
    $.mixin({
        droppable: function(options) {
            return this.each(function(el) {
                new Droppable(el, options);
            });
        }
    });
}), 

Core(function(scope) {
    function Timer(root, options) {
        options = options || {}, this.duration = options.duration, this.curTime = this.duration, 
        this.$el = $(root), this.elements = {
            $value: "#value",
            $progress: "#progress"
        }, this.proxies = [ "tick" ], this.__super__.constructor.call(this), this.$value.text(this.duration), 
        defHeight = this.$progress.attr("height");
    }
    var defHeight;
    $.inherit(Timer, scope.Controller), $.extend(Timer.prototype, {
        from: function(time) {
            this.$value.text(this.curTime = Math.round(time / 1e3));
        },
        start: function() {
            this.$progress.move({
                attributeName: "height",
                to: "0",
                dur: this.curTime
            }), this.timerId = this.withDelay(this.tick, 1e3);
        },
        tick: function() {
            return this.paused ? void (this.timerId = this.withDelay(this.tick, 1e3)) : void (this.curTime-- > 0 && (this.$value.text(this.curTime), 
            this.timerId = this.withDelay(this.tick, 1e3)));
        },
        reset: function() {
            this.$progress.stop().attr({
                height: defHeight
            }), this.$value.text(this.curTime = this.duration), clearTimeout(this.timerId);
        },
        pause: function() {
            this.paused = !0, this.$progress.pause();
        },
        resume: function() {
            this.paused = !1, this.$progress.resume();
        }
    }), 
	scope.Timer = Timer;
}), 

Core(function(scope) {
    function Player(options) {
        options = options || {}, this.name = options.name, this.position = options.position, 
        this.noSkin = options.noSkin, this.skin = options.skin || scope.SKIN_NAMES[$.rand(0, scope.SKIN_NAMES.length - 1)], 
        this.$el = $("#player-" + this.position), this.elements = {
            $timer: "#timer",
            $name: "#name",
            $nameWrapper: "#name rect",
            $nameText: "#name text"
        }, this.proxies = [ "loadSkin" ], this.__super__.constructor.call(this), this.$el.show(), 
        this.$timer.hide(), this.$name.show(), this.$nameText.text(this.name);
        var nameWrapperWidth = this.$nameText.width() / $("#Page-1").width() * 100 + 2 * (this.$nameText.position().left - this.$nameWrapper.position().left) / $("#Page-1").width() * 100;
        this.$nameWrapper.attr({
            width: nameWrapperWidth + "%"
        }), this.initTimer(), "me" == this.position || this.noSkin || $.load("svg/" + [ "person", this.position, this.skin ].join("-") + ".svg", this.loadSkin);
    }
    $.inherit(Player, scope.Controller), $.extend(Player.prototype, {
        loadSkin: function(result) {
            var $result = $("<g/>").html(result);
            this.$el.append($result[0].firstChild), this.unselect();
        },
        initTimer: function() {
            this.timer = new scope.Timer(this.$timer, {
                duration: 30
            });
        },
        select: function() {
            this.$nameWrapper.attr({
                fill: "#517ECE"
            }), this.$nameText.attr({
                fill: "#FFFFFF"
            }), this.$timer.show(), this.$("#selection").show(), this.timer.start();
        },
        unselect: function() {
            this.$nameWrapper.attr({
                fill: "#FFFFFF"
            }), this.$nameText.attr({
                fill: "#48AF5E"
            }), this.$timer.hide(), this.$("#selection").hide(), this.timer.reset();
        }
    }), scope.Player = Player;
}), 

Core(function(scope) {
    function Hand(root, options) {
        options = options || {}, this.$el = $(root), this.elements = {
            $topCard: "#top-card",
            $rollUp: "#roll-up",
            $rollDown: "#roll-down",
            $container: "#container",
            $back: "#back",
            $cards: "#cards"
        }, this.proxies = [ "show", "hide" ], this.__super__.constructor.call(this), this.$rollUp.on("click", this.show), 
        this.$rollDown.on("click", this.hide), this.cards = [];
    }
    var trfsMap = [ {
        backHeight: 74,
        containerTrf: "0 -44",
        cardTrf: "0 -38"
    }, {
        backHeight: 124,
        containerTrf: "0 -94",
        cardTrf: "0 -89"
    }, {
        backHeight: 174,
        containerTrf: "0 -144",
        cardTrf: "0 -139"
    } ], 
    defCardTrf = "0 20";
    $.inherit(Hand, scope.Controller), $.extend(Hand.prototype, {
        take: function() {
            var card = this.cards.pop();
            return this.render(), card && card.$el.remove(), card;
        },
        discard: function(tile) {
            var card = new scope.Card({
                color: scope.CARD_COLORS[tile[1] - 1],
                value: tile[2]
            });
            this.cards.push(card), this.render(), card.drag(), card.dragHandler.disable(), this.$topCard.append(card.$el[0]);
        },
        pop: function() {
            this.cards.pop(), this.render();
        },
        show: function() {
            var count = this.cards.length - 1;
            count > 0 && (this.$container.transform({
                to: trfsMap[4 > count ? count - 1 : 2].containerTrf,
                from: "0 50",
                dur: .355,
                calcMode: "linear"
            }), this.$back.animate({
                attributeName: "height",
                to: trfsMap[4 > count ? count - 1 : 2].backHeight,
                dur: .355
            }), this.$cards.find("g").each(function(el, i) {
                3 > i && $(el).transform({
                    to: trfsMap[i].cardTrf,
                    from: "0 20",
                    dur: .6,
                    calcmode: "linear"
                });
            }), this.shown = !0);
        },
        hide: function() {
            var count = this.cards.length - 1;
            count > 0 && (this.$container.transform({
                to: "0 50",
                from: trfsMap[4 > count ? count - 1 : 2].containerTrf,
                dur: .5,
                calcMode: "linear"
            }), this.$back.animate({
                attributeName: "height",
                to: 0,
                dur: .5
            }), this.$cards.find("g").each(function(el, i) {
                3 > i && $(el).transform({
                    to: defCardTrf,
                    from: trfsMap[4 > count ? count - 1 : 2].cardTrf,
                    dur: .6,
                    calcmode: "linear"
                });
            }), this.shown = !1);
        },
        render: function() {
            var history = this.cards.slice(0, this.cards.length - 1);
            this.$cards.empty(), this.shown && this.hide(), history.reverse().forEach(function(card) {
                card = card.clone(), card.template(scope.CARD_SMALL_SOURCE), card.$el.attr("transform", "translate(" + defCardTrf + ")"), 
                this.$cards.append(card.$el[0]);
            }, this);
        },
        clear: function() {
            this.cards.forEach(function(card) {
                card.$el.remove();
            }), this.cards = [], this.render();
        }
    }), scope.Hand = Hand;
}), 

Core(function(scope) {
    function Card(options) {
        options = options || {}, this.value = options.value, this.color = options.color, 
        this.selected = !1, this.pos = {}, this.elements = {
            $circle: "circle",
            $text: "text",
            $overlay: ".overlay"
        }, this.proxies = [ "toggle", "selectGroup", "dragGroup", "clearGroup", "revertGroup" ], 
        this.template(scope.CARD_SOURCE), this.$el.on("mousedown", this.toggle), this.$el.on("revert", this.revertGroup), 
        this.$el.on("dragstop", this.clearGroup), this.$overlay.on("mousedown", this.selectGroup);
    }
    var selStart = !1;
    $(window).on("keydown", function(e) {
        e.ctrlKey && (selStart = !0);
    }), $(window).on("keyup", function() {
        selStart = !1;
    }), Card.selected = [], Card.uncheckAll = function() {
        for (var i = Card.selected.length; i--; ) Card.selected[i].uncheck();
    }, $("body").on("mousedown", Card.uncheckAll), $.inherit(Card, scope.Controller), 
    $.extend(Card.prototype, {
        template: function(source) {
            $.load(source, this.proxy(this.load));
        },
        load: function(result) {
            this.$el = (this.$el || $("<g/>")).attr({
                "class": "card"
            }).css({
                cursor: "default"
            }).html(result), this.__super__.constructor.call(this), this.render();
        },
        render: function() {
            null == this.value && null == this.color ? (this.$circle.hide(), this.$text.hide()) : 0 == this.value ? (this.$circle.hide(), 
            this.$text.show().attr({
                fill: this.color,
                y: 36
            }).text("*")) : (this.$circle.show().attr("fill", this.color), this.$text.show().attr("fill", this.color).text(this.value));
        },
        drag: function() {
            this.dragHandler = new scope.Draggable(this.$el), this.dragHandler.owner = this;
        },
        centerX: function() {
            var pos = this.$el.position(), width = this.$el.width();
            return Math.round(pos.left + width / 2);
        },
        toggle: function(e) {
            e.stopPropagation(), selStart ? this.nearSelectGroup() ? this.selected && !this.betweenSelected() ? this.uncheck() : this.check() : (Card.uncheckAll(), 
            this.check()) : ~Card.selected.indexOf(this) || Card.uncheckAll();
        },
        check: function() {
            this.$overlay.css("display", ""), this.selected = !0, deck.cards[this.pos.y][this.pos.x] = null, 
            Card.selected.push(this);
        },
        uncheck: function() {
            this.$overlay.css("display", "none"), this.selected = !1, deck.cards[this.pos.y][this.pos.x] = this;
            var i;
            ~(i = Card.selected.indexOf(this)) && Card.selected.splice(i, 1);
        },
        selectGroup: function(e) {
            if (!selStart) {
                for (var card, i = 0, l = Card.selected.length; l > i; i++) card = Card.selected[i], 
                card != this && card.dragHandler.onDown(e, !0);
                this.$el.on("dragmove", this.dragGroup);
            }
        },
        dragGroup: function(e) {
            for (var card, i = 0, l = Card.selected.length; l > i; i++) card = Card.selected[i], 
            card != this && card.dragHandler.onMove(e.detail.event, !0);
        },
        clearGroup: function() {
            this.$el.off("dragmove", this.dragGroup), selStart || Card.uncheckAll();
        },
        nearSelectGroup: function() {
            return Card.selected.some(function(card) {
                return Math.abs(card.pos.x - this.pos.x) <= 1 && card.pos.y == this.pos.y;
            }, this);
        },
        betweenSelected: function() {
            Card.selected.sort(function(a, b) {
                return (a.pos.x < b.pos.x) - (b.pos.x < a.pos.x);
            });
            var idx = Card.selected.indexOf(this);
            return -1 != idx && 0 != idx && idx != Card.selected.length - 1;
        },
        revertGroup: function() {
            for (var card, i = 0, l = Card.selected.length; l > i; i++) card = Card.selected[i], 
            card != this && card.$el.transform({
                from: [ card.dragHandler.dx, card.dragHandler.dy ].join(" "),
                to: card.dragHandler.initTrf.join(" ")
            }), scope.deck.cards[card.pos.y][card.pos.x] = card;
        },
        clone: function() {
            return new Card({
                color: this.color,
                value: this.value
            });
        },
        log: function() {}
    }), scope.Card = Card;
}), 

Core(function(scope) {
    function Deck(root, options) {
        options = options || {}, this.$el = $(root), this.elements = {
            $dropPlace: "#deck"
        }, this.proxies = [ "select", "track", "place" ], this.cards = [ [], [] ], this.trfs = [ [], [] ], 
        this.__super__.constructor.call(this), this.$dropPlace.droppable({
            accept: this.place
        });
    }
    var padding = {
        top: 5,
        left: 5
    };
    $.inherit(Deck, scope.Controller);
    var prevX, prevY, selected;
    $.extend(Deck.prototype, {
        each: function(callback) {
            for (var i = 0; 15 > i; i++) for (var j = 0; 2 > j; j++) if (callback.call(this, this.cards[j][i], i, j) === !1) return;
        },
        fill: function(tiles) {
            var count = 0;
            this.each(function(c, i, j) {
                if (null != c && (this.cards[j][i] = null, c.$el.remove()), 0 == j && count < tiles.length) {
                    var tile = tiles[count], card = new scope.Card({
                        color: scope.CARD_COLORS[tile[1] - 1],
                        value: tile[2]
                    });
                    card.pos = {
                        x: i,
                        y: j
                    }, card.on("dragstart", this.select), card.on("dragmove", this.track), this.cards[j][i] = card, 
                    count++;
                }
                this.trfs[j][i] = {
                    x: 42 * i + padding.left,
                    y: 71 * j + padding.top
                };
            });
        },
        render: function() {
            this.each(function(card, i, j) {
                null != card && (card.$el.attr("transform", "translate(" + this.trfs[j][i].x + " " + this.trfs[j][i].y + ")"), 
                card.drag(), this.$el.append(card.$el[0]));
            });
        },
        track: function(e) {
            this.each(function(card, i, j) {
                if (card && card.$el[0] != e.target && card.intersect(e.detail.x, e.detail.y) && !card.$el.attr("animated") && scope.Card.selected.length < 2) {
                    var shift = e.detail.x > card.centerX() ? i + 1 : i - 1;
                    return shift = shift > 14 ? shift - 2 : shift, shift = 0 > shift ? shift + 2 : shift, 
                    prevX = i, prevY = j, this.move({
                        i: i,
                        j: j
                    }, {
                        i: shift,
                        j: j
                    }), !1;
                }
            });
        },
        move: function(fst, snd) {
            var card, trfs = this.trfs, i = snd.i, cond = function(i) {
                return fst.i < snd.i ? 14 >= i : i >= 0;
            }, _cond = function(j, i) {
                return fst.i < snd.i ? i > j : j > i;
            }, op = function() {
                fst.i < snd.i ? i++ : i--;
            }, _op = function() {
                fst.i < snd.i ? j++ : j--;
            }, direction = function(j) {
                return fst.i < snd.i ? j + (scope.Card.selected.length || 1) : j - (scope.Card.selected.length || 1);
            }, _direction = function(j) {
                return fst.i < snd.i ? j - 1 : j + 1;
            };
            if (scope.Card.selected.length < 2) for (;cond(i); op()) if (null == this.cards[snd.j][i] || this.cards[snd.j][i] == selected) {
                for (var j = _direction(i); _cond(j, i); _op()) card = this.cards[fst.j][j], card != selected && (card.$el.transform({
                    from: [ trfs[fst.j][j].x, trfs[fst.j][j].y ].join(" "),
                    to: [ trfs[fst.j][direction(j)].x, trfs[fst.j][direction(j)].y ].join(" ")
                }), selected && (selected.dragHandler.initTrf = [ trfs[fst.j][j].x, trfs[fst.j][j].y ]), 
                ((this.cards[fst.j][j] = this.cards[fst.j][direction(j)]) || {}).pos = {
                    x: j,
                    y: fst.j
                }, (this.cards[fst.j][direction(j)] = card).pos = {
                    x: direction(j),
                    y: fst.j
                });
                break;
            }
        },
        select: function(e) {
            selected = null, this.each(function(card) {
                return card && card.$el[0] == e.target ? (selected = card, !1) : void 0;
            });
        },
        place: function(target, x, y) {
            var trfs = this.trfs, pos = this.$dropPlace.position(), width = this.$dropPlace.width(), height = this.$dropPlace.height(), placeWidth = Math.round(width / 15), placeHeight = Math.round(height / 2), truePosX = Math.floor((x - pos.left) / placeWidth), posY = Math.floor((y - pos.top) / placeHeight);
            scope.Card.selected.sort(function(a, b) {
                return (a.pos.x > b.pos.x) - (b.pos.x > a.pos.x);
            });
            var dropResult, idx = scope.Card.selected.indexOf(target.owner), cards = scope.Card.selected.length ? scope.Card.selected.concat() : [ target.owner ];
            if (cards.every(function(card, i) {
                return posX = truePosX + (i - idx) * (card != target.owner), null == this.cards[posY][posX] || this.cards[posY][posX] == card;
            }, this)) for (var card, i = 0, l = cards.length; l > i; i++) card = cards[i], posX = truePosX + (i - idx) * (card != target.owner), 
            (dropResult = null == this.cards[posY][posX] || this.cards[posY][posX] == selected) && (this.cards[posY][posX] != card && (null != card.pos.x && null != card.pos.y ? this.cards[card.pos.y][card.pos.x] = this.cards[card.pos.y][card.pos.x] == card ? null : this.cards[card.pos.y][card.pos.x] : (this.$el.trigger("take", {
                detail: {
                    card: card
                }
            }), this.justTaken = !0), (this.cards[posY][posX] = card).pos = {
                x: posX,
                y: posY
            }), function(card) {
                card.$el.transform({
                    from: card.$el.attr("transform").slice(10, -1),
                    to: [ trfs[posY][posX].x, trfs[posY][posX].y ].join(" ")
                }).then(function() {
                    card.dragHandler.storeTrf();
                });
            }(card));
            return dropResult;
        },
        remove: function(tile) {
            return selected && selected.color == scope.CARD_COLORS[tile[1] - 1] && selected.value == tile[2] ? (this.cards[selected.pos.y][selected.pos.x] = null, 
            selected.$el.remove(), selected.off("dragmove", this.track), !1) : void this.each(function(card, i, j) {
                return card && card.color == scope.CARD_COLORS[tile[1] - 1] && card.value == tile[2] ? (this.cards[j][i] = null, 
                card.$el.remove(), card.off("dragmove", this.track), !1) : void 0;
            });
        },
        insert: function(tile) {
            this.justTaken ? this.justTaken = !1 : this.each(function(card, i, j) {
                return card ? void 0 : (card = new scope.Card({
                    color: scope.CARD_COLORS[tile[1] - 1],
                    value: tile[2]
                }), card.pos = {
                    x: i,
                    y: j
                }, card.on("dragstart", this.select), card.on("dragmove", this.track), this.cards[j][i] = card, 
                card.$el.attr("transform", "translate(" + this.trfs[j][i].x + " " + this.trfs[j][i].y + ")"), 
                card.drag(), this.$el.append(card.$el[0]), !1);
            });
        },
        length: function() {
            var count = 0;
            return this.each(function(card) {
                null != card && (count += 1);
            }), count;
        },
        hand: function(discarded) {
            return result = [ [], [] ], this.each(function(card, i, j) {
                card && card != discarded && result[j].push(tuple(atom("OkeyPiece"), scope.CARD_COLORS.indexOf(card.color) + 1, card.value));
            }), result;
        },
        dir: function() {
            for (var i = 0; 15 > i; i++) this.cards[0][i] && this.cards[0][i].log();
            for (var i = 0; 15 > i; i++) this.cards[1][i] && this.cards[1][i].log();
        }
    }), scope.deck = new Deck("#deck-root");
}), 

Core(function(scope) {
    $.load(scope.CARD_SOURCE, function() {
        function fadeOut() {
            $(this).animate({
                attributeName: "opacity",
                from: 1,
                to: 0,
                dur: .3
            });
        }
        function fadeIn() {
            $(this).animate({
                attributeName: "opacity",
                from: 0,
                to: 1,
                dur: .3
            });
        }
        function addFadeOut() {
            $(this).on(document.createTouch ? "touchend" : "mouseup", fadeOut);
        }
        function removeFadeOut() {
            $(this).off(document.createTouch ? "touchend" : "mouseup", fadeOut);
        }
        function createCentralCard() {
            centralCard = new scope.Card(), centralCard.$el.attr({
                opacity: 0,
                transform: "translate(298 -115)"
            }).on(document.createTouch ? "touchstart" : "mousedown", fadeIn).on(document.createTouch ? "touchend" : "mouseup", fadeOut), 
            centralCard.drag(), centralCard.dragHandler.enable(), centralCard.on("dragstart", deck.select).on("dragmove", removeFadeOut).on("dragstop", addFadeOut).on("dragmove", deck.track).on("revert", fadeOut), 
            deck.$el.append(centralCard.$el[0]);
        }
        function init(e) {
            if (ended = !1, scope.deck.fill(e.detail.tiles), scope.deck.render(), centralCard.dragHandler.disable(), 
            centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut), 
            e.detail.gosterge && "null" != e.detail.gosterge) {
                var gosterge = new scope.Card({
                    color: scope.CARD_COLORS[e.detail.gosterge[1] - 1],
                    value: e.detail.gosterge[2]
                });
                gosterge.$el.attr("transform", "translate(16 0)"), $gosterge.append(gosterge.$el);
            }
            var piles = e.detail.piles;
            if (piles && "null" != piles) for (var i = 0; i < piles.length, pile; i++) {
                pile = piles[i];
                for (var name in pile) for (var playerPile = pile[name], hand = playersLeftHandsMap[name], j = playerPile.length; j--; ) hand.discard(playerPile[j]);
            }
            e.detail.whos_move && "null" != e.detail.whos_move && (e.detail.next_turn_in && "null" != e.detail.next_turn_in && playersMap[e.detail.whos_move].timer.from(e.detail.next_turn_in), 
            e.detail.paused && (playersMap[e.detail.whos_move].timer.pause(), $overlay.show()), 
            playersMap[e.detail.whos_move].select());
        }
        window.deck = scope.deck;
        var centralCard, apiProvider = new scope.ApiProvider({
            url: scope.apiUrl,
            gameId: scope.gameId,
            sessionId: scope.defaultSessionId
        });
        createCentralCard(), deck.on("take", function(e) {
            e.detail.card.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut), 
            centralCard.off("dragmove", removeFadeOut).off("dragstop", addFadeOut).off("revert", fadeOut), 
            ~playersLeftHandsMap[scope.user].cards.indexOf(e.detail.card) && playersLeftHandsMap[scope.user].pop(), 
            apiProvider.actionTake(e.detail.card);
        });
        var $gosterge = $("#gosterge"), ended = !0;
        apiProvider.on("okey_game_started", init), apiProvider.on("okey_game_player_state", init);
        var playersPositions = [ [ "me", "right", "center", "left" ], [ "left", "me", "right", "center" ], [ "center", "left", "me", "right" ], [ "right", "center", "left", "me" ] ], playersMap = {}, playersRightHandsMap = {}, playersLeftHandsMap = {};
        apiProvider.on("okey_game_info", function(e) {
            if (!scope.started) {
                for (var playerInfo, players = e.detail.players, i = 0; i < players.length; i++) if (playerInfo = players[i].PlayerInfo, 
                playerInfo[0] == scope.user) {
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
                    playersLeftHandsMap[prevPlayer.PlayerInfo[0]] = playersRightHandsMap[playerInfo[0]] = new scope.Hand("#" + [ "player", playersPositions[i], "hand" ].join("-")), 
                    "me" == playersPositions[i] && playersRightHandsMap[playerInfo[0]].$el.droppable({
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
        }), window.playersRightHandsMap = playersRightHandsMap, window.playersLeftHandsMap = playersLeftHandsMap;
        var playerTurn = !1;
        apiProvider.on("okey_next_turn", function(e) {
            for (var playerName in playersMap) playersMap[playerName].unselect();
            if (playersMap[e.detail.player].select(), e.detail.player == scope.user) {
                playerTurn = !0;
                var cards = playersLeftHandsMap[e.detail.player].cards;
                if (cards.length) {
                    var card = cards[cards.length - 1];
                    deck.$el.append(card.$el[0]), card.$el.attr({
                        transform: "translate(16 -65)"
                    }), card.dragHandler.enable(), card.on("dragstart", deck.select), card.on("dragmove", deck.track);
                }
                deck.length() < 15 ? (centralCard.dragHandler.enable(), centralCard.$el.on(document.createTouch ? "touchstart" : "mousedown", fadeIn).on(document.createTouch ? "touchend" : "mouseup", fadeOut), 
                centralCard.on("dragmove", removeFadeOut).on("dragstop", addFadeOut).on("revert", fadeOut)) : (centralCard.dragHandler.disable(), 
                centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut));
            } else playerTurn = !1, centralCard.dragHandler.disable(), centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut);
        }), apiProvider.on("okey_tile_discarded", function(e) {
            if ("object" == typeof e.detail.tile) {
                var c = new scope.Card({
                    color: scope.CARD_COLORS[e.detail.tile[1] - 1],
                    value: e.detail.tile[2]
                });
                c.log();
            }
            e.detail.player == scope.user && deck.remove(e.detail.tile), playersRightHandsMap[e.detail.player].discard(e.detail.tile);
        });
        var $pile = $("#pile"), $fullPile = $pile.find("g").clone(), $wholeCards = $("#whole-cards"), $fullWholeCards = $("#whole-cards > g").clone();
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
            0 === e.detail.pile) {
                var $topCard = $pile.find("g");
                if ($topCard.length > 1) $topCard.last().remove(); else {
                    $topCard.last().remove(), $pile.append($fullPile);
                    var $miniPile = $wholeCards.find("g");
                    $miniPile.length ? $miniPile.first().remove() : $wholeCards.append($fullWholeCards);
                }
            }
            e.detail.player == scope.user && deck.insert(e.detail.revealed), centralCard.dragHandler.disable(), 
            centralCard.$el.off(document.createTouch ? "touchstart" : "mousedown", fadeIn).off(document.createTouch ? "touchend" : "mouseup", fadeOut);
            var cards = playersLeftHandsMap[scope.user].cards;
            if (cards.length) {
                var card = cards[cards.length - 1];
                card.dragHandler.disable();
            }
        }), apiProvider.on("okey_revealed", function(e) {
            ended = !0, alert(e.detail.player), deck.fill([]);
            for (var hand in playersLeftHandsMap) playersLeftHandsMap[hand].clear();
            for (var playerName in playersMap) playersMap[playerName].unselect();
            $gosterge.remove();
        }), apiProvider.on("player_left", function(e) {
            var playerInfo = e.detail.replacement.PlayerInfo;
            playersMap[playerInfo[0]] = new scope.Player({
                position: playersMap[e.detail.player].position,
                name: [ playerInfo[2], playerInfo[3] ].join(" "),
                noSkin: !0
            }), delete playersMap[e.detail.player], playersRightHandsMap[playerInfo[0]] = playersRightHandsMap[e.detail.player], 
            delete playersRightHandsMap[e.detail.player], playersLeftHandsMap[playerInfo[0]] = playersLeftHandsMap[e.detail.player], 
            delete playersLeftHandsMap[e.detail.player];
        }), $("#pause").on("click", function() {
            apiProvider.pause();
        });
        var whoPausedGame, $overlay = $("#overlay");
        $overlay.on("click", function() {
            whoPausedGame == scope.user && apiProvider.pause(!0);
        }), apiProvider.on("game_paused", function(e) {
            if (whoPausedGame = e.detail[3], "pause" == e.detail[2]) {
                $overlay.show();
                for (var player in playersMap) playersMap[player].timer.pause();
                var player = playersMap[e.detail[3]];
                $overlay.find("text").text(player.name + " paused the game");
            } else {
                $overlay.hide();
                for (var player in playersMap) playersMap[player].timer.resume();
            }
        }), $("#table-ellipse").droppable({
            accept: function(target) {
                return 1 === apiProvider.socket.readyState && deck.length() > 14 && target.owner != centralCard && !ended && scope.Card.selected.length <= 1;
            },
            drop: function(target) {
                apiProvider.reveal(target.owner, deck.hand(target.owner));
            }
        });
    });
    var leftFlag = !1;
    $("#Left-Menu").css("cursor", "pointer").on("click", function() {
        leftFlag ? ($("#Tournaments").transform({
            to: "10 575",
            from: "44 465"
        }), $("#Promos").transform({
            to: "10 575",
            from: "122 538"
        }), leftFlag = !1) : ($("#Tournaments").transform({
            from: "10 575",
            to: "44 465"
        }), $("#Promos").transform({
            from: "10 575",
            to: "122 538"
        }), leftFlag = !0);
    });
    var rightFlag = !1;
    $("#Right-Menu").css("cursor", "pointer").on("click", function() {
        rightFlag ? ($("#Play").transform({
            to: "975 575",
            from: "946, 461"
        }), $("#Create").transform({
            to: "975 575",
            from: "864 526"
        }), rightFlag = !1) : ($("#Play").transform({
            from: "975 575",
            to: "946, 461"
        }), $("#Create").transform({
            from: "975 575",
            to: "864 526"
        }), rightFlag = !0);
    });
});