function ControllerScope(scope) {

    function Controller() {
        this.proxyAll();
        this.refreshElements();
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
    }),

    scope.Controller = Controller;
}
