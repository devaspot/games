
Core(function(scope) {

    function Timer(root, options)
    {
        options = options || {};
        this.duration = options.duration;
        this.curTime = this.duration;
        this.$el = $(root);
        this.elements = {
            $value: "#value",
            $progress: "#progress"
        };
        this.proxies = [ "tick" ];
        this.__super__.constructor.call(this);
        this.$value.attr({x:7,y:26});
        this.$value.text(this.duration);
        defHeight = this.$progress.attr("height");
    }

    var defHeight;

    $.inherit(Timer, scope.Controller);

    $.extend(Timer.prototype, {
        from: function(time) {
            this.$value.text(this.curTime = Math.round(time / 1e3));
        },
        start: function() {
            this.$progress.move({
                attributeName: "height",
                to: "0",
                dur: this.curTime
            }),
            this.timerId = this.withDelay(this.tick, 1e3);
        },
        tick: function() {
            return this.paused 
                ? void (this.timerId = this.withDelay(this.tick, 1e3))
                : void (this.curTime-- > 0 && (this.$value.text(this.curTime),
                        this.timerId = this.withDelay(this.tick, 1e3))
                    );
        },
        reset: function() {
            this.$progress.stop().attr({
                height: defHeight
            }),
            this.$value.text(this.curTime = this.duration),

            clearTimeout(this.timerId);
        },
        pause: function() {
            this.paused = !0, this.$progress.pause();
        },
        resume: function() {
            this.paused = !1, this.$progress.resume();
        }
    });

    scope.Timer = Timer;
});

