function HandScope(scope) {

    function Hand(root, options)
    {
        options = options || {};
        this.$el = $(root);
        this.elements = {
            $topCard: "#top-card",
            $rollUp: "#roll-up",
            $rollDown: "#roll-down",
            $container: "#container",
            $back: "#back",
            $cards: "#cards"
        };
        this.proxies = [ "show", "hide" ];
        this.__super__.constructor.call(this);
        this.$rollUp.on("click", this.show);
        this.$rollDown.on("click", this.hide);
        this.cards = [];
    }

    var trfsMap = [ { backHeight: 74,  containerTrf: "0 -44",  cardTrf: "0 -38"  },
                    { backHeight: 124, containerTrf: "0 -94",  cardTrf: "0 -89"  },
                    { backHeight: 174, containerTrf: "0 -144", cardTrf: "0 -139" } ];

    var defCardTrf = "0 20";

    $.inherit(Hand, scope.Controller);

    $.extend(Hand.prototype, {

        take: function() {
            var card = this.cards.pop();
            return this.render(), card && card.$el.remove(), card; },

        discard: function(tile) {
            var card = new scope.Card({
                color: scope.CARD_COLORS[tile[1]-1],
                value: tile[2]
            });
            this.cards.push(card), this.render(), card.drag(), card.dragHandler.disable(), this.$topCard.append(card.$el[0]); },

        pop: function() { this.cards.pop(), this.render();},

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
            }), this.shown = !0); },

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
            }), this.shown = !1); },

        render: function() {
            var history = this.cards.slice(0, this.cards.length - 1);
            this.$cards.empty(), this.shown && this.hide(), history.reverse().forEach(function(card) {
                card = card.clone(), card.template(scope.CARD_SMALL_SOURCE),
                card.render(),
                (card.value == 0 && card.$text.attr({"font-size":"20pt",y:33})),
                card.$joker.attr({"font-size":"16pt",y:40,x:0}),
                card.$el.attr("transform", "translate(" + defCardTrf + ")"), 
                this.$cards.append(card.$el[0]);
            }, this); },

        clear: function() {
            this.cards.forEach(function(card) {
                card.$el.remove();
            }), this.cards = [], this.render(); }

    });

    scope.Hand = Hand;
}
