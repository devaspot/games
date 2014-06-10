
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
        var msg = e.detail, id = msg[0], name = msg[1], surname = msg[2];
        if (null != document.getElementById(id)) removeOnlineUser(id);
        addOnlineUser(id,name+" "+surname,"insertTop");
        showOnlineList();
    });

    scope.apiProvider.on("offline", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var msg = e.detail, id = msg[0], name = msg[1], surname = msg[2];
        if (null != document.getElementById(id)) removeOnlineUser(id);
        addOnlineUser(id,name+" "+surname,"appendChild");
    });

    scope.apiProvider.on("roster_item", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var msg = e.detail, id = msg[0], name = msg[1], surname = msg[2];
        addOnlineUser(id,name+" "+surname,"appendChild");
    });

    scope.apiProvider.on("roster_end", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        showOnlineList();
    });

    scope.apiProvider.on("chat_message", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var from = dec(e.raw).value[0][1].value[0][0].value,
            names = dec(e.raw).value[0][1].value[0][1].value,
            to = dec(e.raw).value[0][2].value,
            message = dec(e.raw).value[0][3];
        console.log("Income");
        console.log(message);
        chatMessage(currentChat,"1",from==document.user?"Self":from,/*names+":\n"+*/message);
        onlineHover();
        mouseWheelHandler({'detail':-10000,'wheelDelta':-10000});
        onlineHoverOut();
    });

    scope.apiProvider.on("roster_group", function (x) {
        var e = {detail: x.detail.json, raw: x.detail.bert};
        var list = dec(e.raw).value[0][1];
        for (var i=0;i<list.length;i++) {
            var item = list[i],
                id = item.value[0][0].value,
                names = item.value[0][1].value,
                surnames = item.value[0][2].value;
            addOnlineUser(id,names+" "+surnames+" "+user_count++,'appendChild');
        }
    });

}

