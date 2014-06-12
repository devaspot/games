var scope = {
    apiUrl: "ws://" + window.location.hostname + ":" + window.location.port + "/ws/",
    gameId: 1000001,
    CARD_SOURCE: "svg/Card.svg",
    CARD_SMALL_SOURCE: "svg/Card-Small.svg",
    CARD_COLORS: [ "#CE290F", "#3B5998", "#48AF5E", "#F8E81C" ],
    SKIN_NAMES: [ "Alina", "Gabrielo", "Mustafa" ],
    version: 1106201403
};

var $ = function(_undefind)
{
    var isIE = window.navigator.msPointerEnabled

    function Selector(elements) {
        this.length = elements.length;
        for (var i = 0, l = this.length; l > i; i++) this[i] = elements[i];
    }

    function $(selector) {
        return tag.test(selector)
            ? new Selector([ document.createElementNS("http://www.w3.org/2000/svg", tag.exec(selector)[1]) ])
            : "string" == typeof selector 
                ? new Selector(document.querySelectorAll(selector)) 
                : selector instanceof NodeList 
                    ? new Selector(selector) 
                    : selector instanceof Element 
                        ? new Selector([ selector ]) 
                        : selector.addEventListener 
                            ? new Selector([ selector ]) 
                            : selector instanceof Selector ? selector : new Selector([]); }

    var fn = Selector.prototype;

    fn.each = function(callback) { for (var i = 0, l = this.length; l > i; i++) callback(this[i], i); return this; },
    fn.on = function(eventName, eventHandler) { return this.each(function(el) { el.addEventListener(eventName, eventHandler); }); },
    fn.off = function(eventName, eventHandler) { return this.each(function(el) { el.removeEventListener(eventName, eventHandler); }); },
    fn.trigger = function(eventName, data, raw) { 
        data = data || {}
        return this.each(function(el) {
            if(isIE){
                var event = document.createEvent("CustomEvent");
                event.initCustomEvent(eventName, false, false, data.detail);
                el.dispatchEvent(event)
            }
            else {
                var event = new CustomEvent(eventName, data) 
                el.dispatchEvent(event)
            }
        }); 
    },
    fn.show = function() { return this.css("display", "block"); },
    fn.hide = function() { return this.css("display", "none"); },
    fn.text = function(text) { return null != text ? this.each(function(el) { el.textContent = text; }) : this.length ? this[0].textContent : _undefind; },
    fn.remove = function() { return this.each(function(el) { el.parentNode && el.parentNode.removeChild(el); }); },

    fn.css = function(name, value) {
        if (Object(name) === name) {
            for (var prop in name) this.css(prop, name[prop]);
            return this;
        }
        return null != value ? this.each(function(el) {
            el.style[name] = value;
        }) : this.length ? getComputedStyle(this[0]).getPropertyValue(name) : _undefind;
    },

    fn.html = function(html) {
        return null != html ? this.each(function(el) {
            for (;el.firstChild; ) el.removeChild(el.firstChild);
            var fragment = document.createElement("div");
            fragment.innerHTML = "<svg>" + html + "</svg>";
            for (var svg = fragment.firstChild, node = svg.firstChild; node; ) el.appendChild(node.cloneNode(!0)), 
            node = node.nextSibling;
        }) : this.length ? this[0].innerHTML : _undefind;
    },

    fn.attr = function(name, value) {
        if (Object(name) === name) {
            for (var prop in name) this.attr(prop, name[prop]);
            return this;
        }
        return null != value ? this.each(function(el) {
            el.setAttribute(name, value);
        }) : this.length ? this[0].getAttribute(name) : _undefind;
    },

    fn.removeAttr = function(name) { return this.each(function(el) { el.removeAttribute(name); }); },

    fn.append = function(target) {
        return this.each(function(el) {
            target instanceof Selector ? target.each(function(child) {
                el.appendChild(child);
            }) : el.appendChild(target);
        });
    },

    fn.empty = function() {
        return this.each(function(el) {
            for (;el.firstChild; ) el.removeChild(el.firstChild);
        });
    },

    fn.eq = function(idx) { return new Selector(idx >= this.length ? [] : [ this[idx] ]); },
    fn.find = function(selector) {
        var result = [];
        return this.each(function(el) {
            Array.prototype.push.apply(result, el.querySelectorAll(selector));
        }), new Selector(result);
    },
    fn.parent = function() {
        var result = [];
        return this.each(function(el) {
            result.push(el.parentNode);
        }), new Selector(result);
    },
    fn.first = function() {
        return new Selector(this.length ? [ this[0] ] : []);
    },

    fn.last = function() { return new Selector(this.length ? [ this[this.length - 1] ] : []); },
    fn.clone = function() { var result = []; return this.each(function(el) { result.push(el.cloneNode(!0)); }), new Selector(result); },
    fn.width = function() { return this.length ? this[0].getBoundingClientRect().width : _undefind; },
    fn.height = function() { return this.length ? this[0].getBoundingClientRect().height : _undefind; },
    fn.position = function() {
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
        anims = $.extend({}, defaultAnim, anims)

        this.each(function (el){
            var $el = $(el),
                $anim = $el.find('.anim')

            if($anim.length){
                $anim.attr(anims)
            }
            else {
                $el.append(anim(anims)[0])
                $anim = $el.find('.anim')
            }

            // iOS Safari issue: 'endEvent' not fired
            // $anim.on('endEvent', function(){
            //     $el.attr('transform', 'translate(' +  trf.to + ')')
            // })

            el.timerId = setTimeout(function(){
                $el.attr(anims.attributeName, anims.to)
                $el.removeAttr('animated')
                callbacks.forEach(function(c){c()})

                callbacks = []
            }, isIE ? 0 : parseFloat(anims.dur)*1000)

            $el.attr('animated', true)

            if(!isIE){
                $anim[0].beginElement()
            }
        })

        return thenable
    };

    var animDelay = 62.5;

    fn.move = function(anims) {
        return this.each(function(el) {
            function tick() {
                if (!el.paused) {
                    var cur = from + step;
                    (to > from ? to >= cur : cur >= to)
                        ? ($el.attr(property, cur), from = cur)
                        : clearInterval(el.timer);
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
        keyTimes: "0; 1" },
        trf = function(trf) { return $("<animateTransform/>").attr(trf); };

    fn.transform = function(trfs) {
        var callbacks = [], thenable = {
            then: function(complete) {
                return callbacks.push(complete), thenable;
            }
        };
        trfs = $.extend({}, defaultTrf, trfs)

        this.each(function (el){
            var $el = $(el),
                $anim = $el.find('.trf')

            if($anim.length){
                $anim.attr(trfs)
            }
            else {
                $el.append(trf(trfs)[0])
                $anim = $el.find('.trf')
            }

            // iOS Safari issue: 'endEvent' not fired
            // $anim.on('endEvent', function(){
            //     $el.attr('transform', 'translate(' +  trf.to + ')')
            // })

            el.timerId = setTimeout(function(){
                $el.attr('transform', trfs.type + '(' +  trfs.to + ')')
                $el.removeAttr('animated')
                callbacks.forEach(function(c){c()})

                callbacks = []
            }, isIE ? 0 : parseFloat(trfs.dur)*1000 - 20)

            $el.attr('animated', true)
            if(!isIE){
                $anim[0].beginElement()
            }
        })

        return thenable
    },

    fn.stop = function() {
        return this.each(function(el) {
            $(el).find(".anim, .trf").each(function(anim) {
            anim.endElement(); }), clearTimeout(el.timerId), clearInterval(el.timer); }); },

    fn.pause = function() { return $("svg")[0].pauseAnimations(), this.each(function(el) { el.paused = !0; }); },
    fn.resume = function() { return $("svg")[0].unpauseAnimations(), this.each(function(el) { el.paused = !1; }); };

    fn.doubletap = function(doubleTapHandler, delay){
        delay = (delay == null) ? 300 : delay;
        
        if(document.createTouch){
            return this.on('touchend', function(event){
                var now = new Date().getTime();
         
                // the first time this will make delta a negative number
                var lastTouch = this.__lastTouch || now + 1;
                var delta = now - lastTouch;
                if(delta < delay && 0 < delta){
                    // After we detct a doubletap, start over
                    this.__lastTouch = null;
         
                    if(doubleTapHandler !== null && typeof doubleTapHandler === 'function'){
                        doubleTapHandler(event);
                    }
                }else{
                    this.__lastTouch = now
                }
            });
        }
        else {
            return this.on('dblclick', doubleTapHandler)
        }
        
    }

    var tag = /^<(.+)\/>$/;

    return $.extend = function(target) {
        for (var obj, properties, i = 1, l = arguments.length; l > i; i++) {
            obj = arguments[i], properties = Object.keys(obj);
            for (var property, j = properties.length; j--; ) property = properties[j], target[property] = obj[property];
        }
        return target;
    },

    $.inherit = function(child, parent) {
        function ctor() { this.constructor = child, this.__super__ = parent.prototype; }
        return ctor.prototype = parent.prototype, child.prototype = new ctor(), child; },

    $.mixin = function(plagin) { $.extend(fn, plagin); },

    $.timestamp = scope.version,

    $.load = function(url, complete) {
        url = url + "?q=" + $.timestamp;
        var result = localStorage.getItem(url);
        if (null == result) {
            var xhr = new XMLHttpRequest();
            xhr.open("GET", url, !0), xhr.onload = function() {
                localStorage.setItem(url, xhr.responseText),
                complete(xhr.responseText);
            }, xhr.send();
        } else complete(result);
    },

    $.rand = function(min, max) { return min + Math.floor(Math.random() * (max - min + 1)); },

    $;
}();

Core = function() {
    return function(module) {
        module(scope);
    };
}();
