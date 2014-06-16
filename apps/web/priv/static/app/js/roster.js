
function RosterScope(scope)
{
    function Roster(scope) { RosterHandlers(scope); }
    scope.Roster = Roster;
}

function RosterHandlers(scope) {

    scope.apiProvider.on("online_number", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        document.getElementById("Users-Online-Number").firstElementChild.textContent = e.detail.toString(); 
    });

    scope.apiProvider.on("online", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var id      = dec(e.raw).value[0][1].value;
            name    = dec(e.raw).value[0][2].value;
            surname = dec(e.raw).value[0][3].value;
            score   = dec(e.raw).value[0][4];
        if (null != document.getElementById(id)) removeOnlineUser(id);
        addOnlineUser(id,name+" "+surname,score,"insertTop");
        if (currentChat == null) showOnlineList();
    });

    scope.apiProvider.on("offline", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var id      = dec(e.raw).value[0][1].value;
            name    = dec(e.raw).value[0][2].value;
            surname = dec(e.raw).value[0][3].value;
            score   = dec(e.raw).value[0][4];
        if (null != document.getElementById(id)) removeOnlineUser(id);
        addOnlineUser(id,name+" "+surname,score,"appendChild");
    });

    scope.apiProvider.on("roster_item", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var id      = dec(e.raw).value[0][1].value;
            name    = dec(e.raw).value[0][2].value;
            surname = dec(e.raw).value[0][3].value;
            score   = dec(e.raw).value[0][4];
        addOnlineUser(id,name+" "+surname,score,"appendChild");
    });

    scope.apiProvider.on("roster_end", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        if (currentChat == null) showOnlineList();
        var now = new Date().getTime();
        var page_load_time = now - perfCounter.start;
        console.log(user_count + " users loaded in " + page_load_time + "ms");
    });

    scope.apiProvider.on("chat_message", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var from    = dec(e.raw).value[0][1].value[0][0].value,
            names   = dec(e.raw).value[0][1].value[0][1].value,
            to      = dec(e.raw).value[0][2].value,
            message = dec(e.raw).value[0][3].value;
        chatMessage("Chat+"+from,"1",from==document.user?"Self":from,utf8decode(message));
        if (null != currentChat) {
            onlineHover();
            mouseWheelHandler({'detail':-10000,'wheelDelta':-10000});
            onlineHoverOut();
        }
    });

    scope.apiProvider.on("chat_event", function(x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var gameId  = dec(e.raw).value[0][1];
        var userId  = dec(e.raw).value[0][2].value;
        var name    = dec(e.raw).value[0][3].value;
        var message = dec(e.raw).value[0][4];
        if (userId != document.user)
        {
            chatMessage("Chat","1",userId==document.user?"Self":userId,name+":\n"+utf8decode(message));
            scroll_right = -10000;
            barHover();
            mouseWheelHandler({'detail':-10000,'wheelDelta':-10000});
            barHoverOut();
        }
    });

    scope.apiProvider.on("stats_event", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        document.getElementById('Player-Statistics').style.display = '';
        var games    = dec(e.raw).value[0][2],
            reveals  = dec(e.raw).value[0][3],
            protocol = dec(e.raw).value[0][4];
            score    = dec(e.raw).value[0][5];
        removeChilds(document.getElementById('Stat-Left'));
        removeChilds(document.getElementById('Stat-Right'));
//            for (var i=0;i<games.length;i++) { statsRow(4, i,games); }
        for (var i=0;i<protocol.length;i++) { statsRow(4,180,i,protocol); }
        for (var i=0;i<reveals.length;i++) { statsRow(320,180,i,reveals); }
        $("#Score").text("Score: " + score).attr({y: 40});
    });

    scope.apiProvider.on("roster_group", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var list = dec(e.raw).value[0][1];
        for (var i=0;i<list.length;i++) {
            user_count++;
            var item = list[i],
                id       = item.value[0][0].value,
                names    = item.value[0][1].value,
                surnames = item.value[0][2].value,
                score    = item.value[0][3];
            addOnlineUser(id,names+" "+surnames,score,'appendChild');
        }
    });

}

