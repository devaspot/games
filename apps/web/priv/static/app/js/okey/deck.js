function DeckScope(scope) {

    function Deck(root, options) {
        options = options || {};
        this.$el = $(root);
        this.elements = { $dropPlace: "#Deck" };
        this.proxies = [ "select", "track", "place" ];
        this.cards = [ [], [] ];
        this.trfs = [ [], [] ];
        this.__super__.constructor.call(this),
        this.$dropPlace.droppable({ accept: this.place });
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
                    },
                    card.on("dragstart", this.select),
                    card.on("dragmove", this.track),
                    card.$el.doubletap(function(){ 
                        scope.apiProvider.actionDiscard(card) 
                    }),
                    this.cards[j][i] = card, 
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
                if (null != card) {
                    card.$el.attr("transform", "translate(" + this.trfs[j][i].x + " " + this.trfs[j][i].y + ")");
//                    console.log(card.$el[0]);
                    card.drag();
                    this.$el.append(card.$el[0]);
                }
            });
        },

        track: function(e) {
            this.each(function(card, i, j) {
                if (card && card.$el[0] != e.target && card.intersect(e.detail.x, e.detail.y) && !card.$el.attr("animated") && scope.Card.selected.length < 2) {
                    var shift = e.detail.x > card.centerX() ? i + 1 : i - 1;
                    return shift = shift > 14 ? shift - 2 : shift, shift = 0 > shift ? shift + 2 : shift, 
                    prevX = i, prevY = j, this.move({ i: i, j: j}, {i: shift, j: j}), !1;
                }
            });
        },

        move: function(fst, snd){
            var trfs = this.trfs,
                card

            var i = snd.i,
                cond = function(i){ return fst.i < snd.i ? i <= 14 : i >= 0 },
                _cond = function(j, i){ return fst.i < snd.i ? (console.log('j < i', j, i), j > fst.i-1) : (console.log('j < i', j, i), j < fst.i+1) },
                op = function(){ fst.i < snd.i ? i++ : i-- },
                _op = function(){ fst.i < snd.i ? j-- : j++ },
                direction = function(j){ return fst.i < snd.i ? j+(scope.Card.selected.length || 1) : j-(scope.Card.selected.length || 1)},
                _direction = function(j){ return fst.i < snd.i ? j-1 : j+1}

            if(scope.Card.selected.length < 2){
                for(; cond(i); op()){
                    console.log('i=', i)
                    if(this.cards[snd.j][i] == null || this.cards[snd.j][i] == selected){
                        var j = _direction(i)
                        for(; _cond(j, i); _op()){
                            console.log('j=', j)
                            card = this.cards[fst.j][j]
                            if(card != selected){

                                card.$el.transform({
                                    from: [trfs[fst.j][j].x, trfs[fst.j][j].y].join(' '),
                                    to: [trfs[fst.j][direction(j)].x, trfs[fst.j][direction(j)].y].join(' ')
                                })

                                if(selected){
                                    selected.dragHandler.initTrf = [trfs[fst.j][j].x, trfs[fst.j][j].y]
                                }

                                ;((this.cards[fst.j][j] = this.cards[fst.j][direction(j)]) || {}).pos = {x:j, y:fst.j}
                                ;(this.cards[fst.j][direction(j)] = card).pos = {x:direction(j), y:fst.j}
                            }
                        }
                        break
                    }
                }
            }
        },

        select: function(e) {
            selected = null, this.each(function(card) {
                return card && card.$el[0] == e.target ? (selected = card, !1) : !0;
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
            if(!this.justTaken){
                this.each(function (card, i, j){
                    if(!card){
                        card = new scope.Card({
                            color: scope.CARD_COLORS[tile[1]-1],
                            value: tile[2]
                        })
                        card.pos = {x:i, y:j}
                        card.on('dragstart', this.select)
                        card.on('dragmove', this.track)
                        card.$el.doubletap(function(){ 
                            scope.apiProvider.actionDiscard(card) 
                        })                     
                        this.cards[j][i] = card
                        card.$el.attr('transform', 'translate(' + this.trfs[j][i].x + ' ' + this.trfs[j][i].y + ')')
                        card.drag()
                        this.$el.append(card.$el[0])
                        return false
                    }
                })
            }
            else {
                this.justTaken = false
            }
        },

        length: function() {
            var count = 0;
            return this.each(function(card) {
                null != card && (count += 1);
            }), count;
        },

        hand: function(discarded) {
            return result = [ [], [] ], this.each(function(card, i, j) {
                if (null == card || card == discarded) result[j].push(atom('null'));
                else result[j].push(tuple(atom("OkeyPiece"),
                        scope.CARD_COLORS.indexOf(card.color) + 1, card.value)); }), result;
        },

        contains: function(card){
            var result = false
            this.each(function(c){
                if(c == card){
                    result = true
                }
            })
            return result
        },

        dir: function() {
            for (var i = 0; 15 > i; i++) this.cards[0][i] && this.cards[0][i].log();
            for (var i = 0; 15 > i; i++) this.cards[1][i] && this.cards[1][i].log();
        }
    });

    scope.deck = new Deck("#Deck-Root",{});

}

