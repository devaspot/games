function PlayerScope(scope) {

    function Player(options)
    {
        options = options || {},
        this.name = options.name;
        this.position = options.position;
        this.noSkin = options.noSkin;
        this.sex = options.sex;
        var skins = this.sex == "female" ? scope.FEMALE_SKINS : scope.MALE_SKINS;
        this.skin = options.skin || skins[$.rand(0, skins.length - 1)];
        this.$el = $("#Player-" + this.position);
        this.elements = {
            $timer:       "#Player-" + this.position + "-Timer",
            $name:        "#Player-" + this.position + "-Display",
            $nameWrapper: "#Player-" + this.position + "-Display rect",
            $nameText:    "#Player-" + this.position + "-Display text" };
        this.proxies = [ "loadSkin" ];
        this.__super__.constructor.call(this);
        this.$el.show();
        this.$timer.hide();
        this.$name.show();

        setPlayerName("Player-" + this.position, this.name);

        this.initTimer();

        "Me" == this.position || this.noSkin || $.load("svg/" + [ "Person", this.position, this.skin ].join("-") + ".svg", this.loadSkin);
    }

    $.inherit(Player, scope.Controller);

    $.extend(Player.prototype, {

        loadSkin: function(result) {

            var $result = $("<g/>").html(result);
            var element = $result[0].firstChild;
/*            var xform = parseTransformAttribute(element.getAttribute("transform"));
            var ori   = parseTransformAttribute(this.$el[0].getAttribute("transform"));
            var shift = "translate("+(-parseFloat(ori.translate[0])+parseFloat(xform.translate[0]))+","+
                                     (-parseFloat(ori.translate[1])+parseFloat(xform.translate[1]))+")";
            element.setAttribute("transform",shift);
*/            this.$el.append(element);
            this.unselect(); },

        initTimer: function() {
            this.timer = new scope.Timer(this.$timer, { duration: 30 }); },

        select: function() {
            this.$nameWrapper.attr({ fill: "#517ECE" }),
            this.$nameText.attr({ fill: "#FFFFFF" }),
            this.$timer.show(),
            this.$("#Selection").show(),
            this.timer.start(); },

        unselect: function() {
            this.$nameWrapper.attr({ fill: "#FFFFFF" }),
            this.$nameText.attr({ fill: "#48AF5E" }),
            this.$timer.hide(),
            this.$("#Selection").hide(),
            this.timer.reset(); }
    });

    scope.Player = Player;
}
