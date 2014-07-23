function ControllerScope(scope) {

    function Controller() {
        this.proxyAll();
        this.refreshElements();
    }

    var isIE = window.navigator.msPointerEnabled;

    $.extend(Controller.prototype, {
        proxy: function(func) { return func.bind(this); },
        proxyAll: function() {
            if (this.proxies) for (var method, i = this.proxies.length; i--; ) method = this.proxies[i], 
            this[method] = this.proxy(this[method]);
        },
        withDelay: function(func, timeout) { return setTimeout(this.proxy(func), timeout || 0); },
        $: function(selector) { return this.$el.find(selector); },
        refreshElements: function() { if (this.elements) for (var element in this.elements) this[element] = this.$(this.elements[element]); },
        on: function(eventType, handler) { return this.$el.on(eventType, handler), this; },
        off: function(eventType, handler) { return this.$el.off(eventType, handler), this; },
        clientX: function(e) { return isIE ? e.pageX : document.createTouch ? e.changedTouches[0].clientX : e.clientX; },
        clientY: function(e) { return isIE ? e.pageY : document.createTouch ? e.changedTouches[0].clientY : e.clientY; },
        intersect: function($el) {
            // var pos = this.$el.position();
            // return pos.top < y && pos.bottom > y && pos.left < x && pos.right > x;
            var d0 = this.$el.position()
            ,   d1 = $el.position()
            ,   x11 = d0.left
            ,   y11 = d0.top
            ,   x12 = d0.left + this.$el.width()
            ,   y12 = d0.top + this.$el.height()
            ,   x21 = d1.left
            ,   y21 = d1.top
            ,   x22 = d1.left + $el.width()
            ,   y22 = d1.top + $el.height()
            ,   x_overlap = Math.max(0, Math.min(x12,x22) - Math.max(x11,x21))
            ,   y_overlap = Math.max(0, Math.min(y12,y22) - Math.max(y11,y21))
            return {
                square: x_overlap * y_overlap
            ,   res : x_overlap * y_overlap > $el.width() * $el.height() * .5
            }   
        }
    }),

    scope.Controller = Controller;
}
