function DeckScope(scope) {

    function Deck(root, options) {
        options = options || {};
        this.$el = $(root);
        this.elements = { $dropPlace: "#Deck" };
        this.proxies = [ "select", "track", "place", 'restoreCards' ];
        this.cards = [ [], [] ];
        this.trfs = [ [], [] ];
        this.__super__.constructor.call(this),
        this.$dropPlace.droppable({ accept: this.place });

        // this.track = $.debounce(this.track, 25)
        // this.restoreCards = $.throttle(this.restoreCards, 200)

        this.restoredCards = [[], []]
        this.selectedPos = {}
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
                    var tile = tiles[count].value[0], card = new scope.Card({
                        color: scope.CARD_COLORS[tile[1] - 1],
                        value: tile[2]
                    });
                    card.pos = {
                        x: i,
                        y: j
                    },
                    card.on("dragstart", this.select),
                    card.on("dragmove", this.track),
                    card.$el.on('revert', this.restoreCards)
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

        restoreCards: function(){
            if(this.restoredCards && this.needRestore){
                var trfs = this.trfs
                for(var i = 0; i < 15; i++){
                    for(var j = 0; j < 2; j++){
                        var card = this.restoredCards[j][i]
                        if(card && card != selected){
                            card.$el.transform({
                                from: [trfs[card.pos.y][card.pos.x].x, trfs[card.pos.y][card.pos.x].y].join(' '),
                                to: [trfs[j][i].x, trfs[j][i].y].join(' ')
                            })

                            card.pos = {
                                x: i,
                                y: j
                            }

                            card.direction = null
                        }
                    }
                }
                this.needRestore = false
                this.cards = this.restoredCards.map(function (row){ return row.concat() })
                this.normalizeCards()
            }
        },

        normalizeCards: function(){
            for(var i = 0; i < 15; i++){
                for(var j = 0; j < 2; j++){
                    var card = this.cards[j][i]
                    if(card){
                        card.pos = {
                            x: i,
                            y: j
                        }
                    }
                }
            }
        },

        dropDiraction: function(){
            for(var i = 0; i < 15; i++){
                for(var j = 0; j < 2; j++){
                    var card = this.cards[j][i]
                    if(card){
                        card.direction = null
                    }
                }
            }
        },

        render: function() {
            this.each(function(card, i, j) {
                if (null != card) {
                    card.$el.attr("transform", "translate(" + this.trfs[j][i].x + " " + this.trfs[j][i].y + ")");
                    card.drag();
                    this.$el.append(card.$el[0]);
                }
            });
        },

        track: function(e) {
            var selectedPos = this.getSelectedPos()
            for(var i = 0; i < 15; i++){
                for(var j = 0; j < 2; j++){
                    var card = this.cards[j][i]
                    if (card && card.$el[0] != e.target && (selectedPos.x == i && selectedPos.y == j) && !card.$el.attr("animated") && scope.Card.selected.length < 2) {
                        var shift = e.detail.x > card.centerX() ? i - 1 : i + 1                        
                        if(shift > 14 || shift < 0){
                            shift = shift > 14 ? shift - 2 : shift
                            shift = 0 > shift ? shift + 2 : shift
                        }
                        else if(shift < i && this.leftDeadEnd(j, i)){
                            shift = i + 1
                        }
                        else if(shift > i && this.rightDeadEnd(j, i)){
                            shift = i - 1
                        }
                        else if(card.direction == this.direction(i, shift)){
                            shift = card.direction == 'left' ? i - 1 : i + 1
                        }
                        this.move({ i: i, j: j}, {i: shift, j: j})
                        this.needRestore = true
                        moved = true
                        prevX = e.detail.x
                        prevY = e.detail.y
                        this.selectedPos = { x: i, y: j}
                        return !1;
                    }
                }
            }
            
            if((this.selectedPos.x != selectedPos.x || this.selectedPos.y != selectedPos.y)){
                this.restoreCards()
            } 
        },

        move: function(fst, snd){
            var trfs = this.trfs,
                card

            var i = snd.i,
                cond = function(i){ return fst.i < snd.i ? i <= 14 : i >= 0 },
                _cond = function(j, i) {
                    return fst.i < snd.i ? (
                    //console.log('j < i', j, i),
                     j > fst.i-1) : (
                     //console.log('j < i', j, i), 
                     j < fst.i+1) },
                op = function(){ fst.i < snd.i ? i++ : i-- },
                _op = function(){ fst.i < snd.i ? j-- : j++ },
                direction = function(j){ return fst.i < snd.i ? j+(scope.Card.selected.length || 1) : j-(scope.Card.selected.length || 1)},
                _direction = function(j){ return fst.i < snd.i ? j-1 : j+1}

            if(scope.Card.selected.length < 2){
                for(; cond(i); op()){
                    //console.log('i=', i)
                    if(this.cards[snd.j][i] == null || this.cards[snd.j][i] == selected){
                        var j = _direction(i)
                        for(; _cond(j, i); _op()){
                            //console.log('j=', j)
                            card = this.cards[fst.j][j]
                            if(card != selected){

                                var self = this

                                ;(function (card, fst_j, j, dir_j, deck){                                 
                                    card.$el.transform({
                                        from: [trfs[fst_j][j].x, trfs[fst_j][j].y].join(' '),
                                        to: [trfs[fst_j][dir_j].x, trfs[fst_j][dir_j].y].join(' ')
                                    })

                                    card.direction = deck.direction(j, dir_j)

                                    // card.restoreCardHandler = function(){
                                    //     var selectedPos = self.getSelectedPos()
                                    //     if(!self.cards[fst_j][j] && (selectedPos.x != j || selectedPos.y != fst_j)){
                                    //         console.log('restoreCard');
                                    //         card.$el.transform({
                                    //             to: [trfs[fst_j][j].x, trfs[fst_j][j].y].join(' '),
                                    //             from: [trfs[fst_j][dir_j].x, trfs[fst_j][dir_j].y].join(' ')
                                    //         })
                                    //         ;(self.cards[fst_j][j] = card).pos = {x:j, y:fst_j}
                                    //         self.cards[fst_j][dir_j] = null
                                    //         card.restoreCardHandler = function(){}
                                    //         var idx = self.restoredCards.indexOf(card)
                                    //         if(~idx){
                                    //             self.restoredCards.splice(idx, 1)
                                    //         }
                                    //     }
                                    // }
                                    // self.restoredCards.push(card)
                                }(card, fst.j, j, direction(j), this))

                                // if(selected){
                                //     selected.dragHandler.initTrf = [trfs[fst.j][j].x, trfs[fst.j][j].y]
                                // }

                                ;((this.cards[fst.j][j] = this.cards[fst.j][direction(j)]) || {}).pos = {x:j, y:fst.j}
                                ;(this.cards[fst.j][direction(j)] = card).pos = {x:direction(j), y:fst.j}
                            }
                        }
                        break
                    }
                }
            }
            this.normalizeCards()
        },

        select: function(e) {
            selected = null;
            selected = e.detail.target.owner
            this.restoredCards = this.cards.map(function (row){ return row.concat() })
            // this.each(function(card) {
            //     if(card && card.$el[0] == e.target){
            //         selected = card
            //         this.restoredCards = this.cards.map(function (row){ return row.concat() })
            //         return !1 
            //     }
            //     else {
            //         return !0
            //     }
            // });
        },

        place: function(target, x, y) {
            try{
                var trfs = this.trfs,
                    posX = this.getSelectedPos().x,
                    posY = this.getSelectedPos().y

                scope.Card.selected.sort(function (a, b){
                    return (a.pos.x > b.pos.x) - (b.pos.x > a.pos.x)
                })

                var idx = scope.Card.selected.indexOf(target.owner),
                    cards = scope.Card.selected.length ? scope.Card.selected.concat() : [target.owner],
                    dropResult

                if(cards.every(function (card, i){
                    return this.cards[posY][posX] == null || this.cards[posY][posX] == card
                }, this)){

                    for(var i = 0, l = cards.length, card; i < l; i++){
                        card = cards[i]

                        if(dropResult = (this.cards[posY][posX] == null || this.cards[posY][posX] == selected)){

                            if(this.cards[posY][posX] != card){
                                if(card.pos.x != null && card.pos.y != null){
                                    this.cards[card.pos.y][card.pos.x] = this.cards[card.pos.y][card.pos.x] == card ? null : this.cards[card.pos.y][card.pos.x]
                                }
                                else {
                                    this.$el.trigger('take', { detail: { card: card }})
                                    this.justTaken = true
                                }
                                ;(this.cards[posY][posX] = card).pos = {x:posX,y:posY}
                                this.normalizeCards()
                                this.dropDiraction()
                                this.restoredCards = null
                                selected = null
                            }

                            ;(function(card){
                                card.$el.transform({
                                    from: card.$el.attr('transform').slice(10,-1),
                                    to: [trfs[posY][posX].x, trfs[posY][posX].y].join(' ')
                                }).then(function(){
                                    card.dragHandler.storeTrf()
                                })
                            }(card))
                        }
                    }
                }
                return dropResult
            }
            catch(e){
                return false
            }
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
                        });
                        card.pos = {x:i, y:j};
                        this.normalizeCards()
                        card.on('dragstart', this.select);
                        card.on('dragmove', this.track);
                        card.$el.doubletap(function() { scope.apiProvider.actionDiscard(card) });
                        this.cards[j][i] = card;
                        card.$el.attr('transform', 'translate(' + this.trfs[j][i].x + ' ' + this.trfs[j][i].y + ')');
                        card.drag();
                        this.$el.append(card.$el[0]);
                        return false;
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

        getSelectedPos: function(){
            if(selected){
                var x = selected.centerX(),
                    y = selected.centerY(),
                    pos = this.$dropPlace.position(),
                    width = this.$dropPlace.width(),
                    height = this.$dropPlace.height(),
                    placeWidth = Math.round(width / 15),
                    placeHeight = Math.round(height / 2),
                    posX = Math.floor((x - pos.left) / placeWidth),
                    posY = Math.floor((y - pos.top) / placeHeight) - 1

                // console.log(posX, posY)
                return {
                    x: posX,
                    y: posY
                }
            }
            else {
                return {
                    x: -1,
                    y: -1
                }
            }
        },

        leftDeadEnd: function(row, from){
            for(var i = from; i >= 0; i--){
                if(!this.cards[row][i] || this.cards[row][i] == selected){
                    return false
                }
            }
            return true
        },

        rightDeadEnd: function(row, from){
            for(var i = from+1; i < 15; i++){
                if(!this.cards[row][i] || this.cards[row][i] == selected){
                    return false
                }
            }
            return true
        },

        direction: function(from, to){
            return from < to ? 'left' : 'right'
        },

        dir: function(){
            console.group('Cards')
            console.group()
            for(var i = 0; i < 15; i++){
                if(this.cards[0][i]){
                    this.cards[0][i].log()
                }
                else {
                    console.log('%c■', 'color: #333')
                }
            }
            console.groupEnd()
            console.group()
            for(var i = 0; i < 15; i++){
                if(this.cards[1][i]){
                    this.cards[1][i].log()
                }
                else {
                    console.log('%c■', 'color: #333')
                }
            }
            console.groupEnd()
            console.groupEnd()
        }
    });

    scope.deck = new Deck("#Deck-Root",{});

}

