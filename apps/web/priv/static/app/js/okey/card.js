function CardScope(scope) {

    function Card(options) {
        options = options || {},
        this.value = options.value,
        this.color = options.color, 
        this.selected = !1,
        this.pos = {},
        this.elements = {
            $circle: "circle",
            $text: "text",
            $overlay: ".overlay"
        },
        this.proxies = [ "toggle", "selectGroup", "dragGroup", "clearGroup", "revertGroup" ], 
        this.template(scope.CARD_SOURCE),
        this.$el.on("mousedown", this.toggle),
        this.$el.on("revert", this.revertGroup), 
        this.$el.on("dragstop", this.clearGroup),
        this.$overlay.on("mousedown", this.selectGroup);
    }

    var selStart = !1;

    $(window).on("keydown", function(e) { e.altKey && (selStart = !0); });
    $(window).on("keyup", function() { selStart = !1; });

    Card.selected = [];
    Card.uncheckAll = function() { for (var i = Card.selected.length; i--; ) Card.selected[i].uncheck(); };

    $("body").on("mousedown", Card.uncheckAll);

    $.inherit(Card, scope.Controller);

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
    });

    scope.Card = Card;
}

