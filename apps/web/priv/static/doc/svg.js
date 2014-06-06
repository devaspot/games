if (!String.prototype.encodeHTML) {
  String.prototype.encodeHTML = function () {
    return this.replace(/&/g, '&amp;')
               .replace(/</g, '&lt;')
               .replace(/>/g, '&gt;')
               .replace(/"/g, '&quot;')
               .replace(/'/g, '&apos;'); // "
  };
}

var transition = {pid: '', port: '8080' };

// BERT Protocol

var players = ["Gabrielo","Mustafa","Alina","Me"];

function statsRow(start_name,i,games) {
    var start_score = 200;
    var name = template_engine(
        '<tspan xmlns="http://www.w3.org/2000/svg" x="{this.x}" y="{this.y}">{this.body}</tspan>',{
            x: start_name,
            y: 180+25*i,
            body: games[i].value[0][0] + " — " + games[i].value[0][1]});
    var element1 = svg(name);
    document.getElementById('Stat-Right').appendChild(element1);
}

var user_count = 0;
function handle_web_socket(body) {
//    console.log(dec(body).value[0][0].value);
    switch (dec(body).value[0][0].value) {
        case 'roster_item':
            var id       = dec(body).value[0][1].value;
            var names    = dec(body).value[0][2].value;
            var surnames = dec(body).value[0][3].value;
            addOnlineUser(id,names+" "+surnames+ " "+user_count++,'appendChild');
            break;
        case 'roster_group':
            var list     = dec(body).value[0][1];
            for (var i=0;i<list.length;i++) {
                var item       = list[i];
                var id       = item.value[0][0].value;
                var names    = item.value[0][1].value;
                var surnames = item.value[0][2].value;
                addOnlineUser(id,names+" "+surnames+ " "+user_count++,'appendChild');
            }
            break;
        case 'roster_end':
            onlineHover();
            mouseWheelHandler({'detail':5,'wheelDelta':5});
            onlineHoverOut();
            document.getElementById("Online-List").style.display = '';
            break;
        case 'online':
            var id = dec(body).value[0][1].value;
            var name = dec(body).value[0][2].value;
            var surname = dec(body).value[0][3].value;
            try{removeOnlineUser(id)}catch(e){}
            addOnlineUser(id,name+" "+surname,"insertTop");
            break;
        case 'chat_message':
            var from = dec(body).value[0][1].value;
            var to = dec(body).value[0][2].value;
            var message = dec(body).value[0][3].value;
            chatMessage(currentChat,"1",from,from+":\n"+message.encodeHTML());
            onlineHover();
            mouseWheelHandler({'detail':-10000,'wheelDelta':-10000});
            onlineHoverOut();
            break;
        case 'offline':
            var id = dec(body).value[0][1].value;
            var name = dec(body).value[0][2].value;
            var surname = dec(body).value[0][3].value;
            try{removeOnlineUser(id)}catch(e){}
            addOnlineUser(id,name+" "+surname,"appendChild");
            break;
        case 'online_number':
            var number = dec(body).value[0][1];
            document.getElementById("722").firstElementChild.textContent = number.toString(); 
            break;
        case 'stats_event':
            document.getElementById('Player-Statistics').style.display = '';
            var games    = dec(body).value[0][2];
            var reveals  = dec(body).value[0][3];
            var protocol = dec(body).value[0][4];
            removeChilds(document.getElementById('Stat-Left'));
            removeChilds(document.getElementById('Stat-Right'));
            for (var i=0;i<games.length;i++) { statsRow(24, i,games); }
            for (var i=0;i<protocol.length;i++) { statsRow(340,i,protocol); }
//            for (var i=0;i<reveals.length;i++) { statsRow(340,i,reveals); }
            break;
    }

    switch (dec(body).value[0][2].value) {
        case 'okey_game_info': 
            var a = dec(body).value[0][3][0].value[0][1];
//            console.log("Players: " + a.length);
            for (var i=0;i<a.length;i++) {
                var c = a[i].value[0][3].value;
                var v = a[i].value[0][4].value;
                setPlayerName(players[i],c + " " + v);
//                console.log("Player: " + c + " " + v);
            }
            break;
        case 'okey_game_started': 
            var a = dec(body).value[0][3][0].value[0][1];
//            console.log("Cards: " + a.length);
            for (var i=1;i<=a.length;i++) {
                var c = a[i-1].value[0][1];
                var v = a[i-1].value[0][2];
//                console.log("Card: " + c + " " + v);
                place_card(i,rand(1,2),c,v);
            }
            break;
        case 'okey_game_player_state':  /// these two messages should be combined
            var a = dec(body).value[0][3][3].value[0][1];
//            console.log("Cards: " + a.length);
            for (var i=1;i<=a.length;i++) {
                var c = a[i-1].value[0][1];
                var v = a[i-1].value[0][2];
//                console.log("Card: " + c + " " + v);
                place_card(i,1,c,v);
            }
            break;
        case 'okey_tile_discarded':
            console.log(String(dec(body)));
            var player = dec(body).value[0][3][0].value[0][1];
            var tile = dec(body).value[0][3][1].value[0][1];
            if (player.value == document.user) {
                console.log("Discard: " + player.value);
                var p = findCardOnTable(tile.value[0][1],tile.value[0][2]);
                empty_card(p.x,p.y);
            }
            break;
        case 'okey_tile_taken': 
            console.log(String(dec(body)));
            var player = dec(body).value[0][3][0].value[0][1];
            var taken = dec(body).value[0][3][2].value[0][1];
            if (taken.value != 'null' && player.value == document.user) {
                console.log("Taken: " + player.value);
                var c = taken.value[0][1];
                var v = taken.value[0][2];
                var pos = findPlace();
//                console.log(pos);
                place_card(pos.x,pos.y,c,v);
            }
            break;
        default:
         console.log(String(dec(body)));
    }
}

// SVG DOM and Template Engine

function template_engine(html, data) {
    var re = /{([^}]+)?}/g, code = 'var r=[];', cursor = 0;
    var add = function(line,js) {
        js? (code += 'r.push(' + line + ');') :
            (code += line != '' ? 'r.push("' + line.replace(/"/g, '\\"') + '");' : ''); // "
        return add; }
    while(match = re.exec(html)) {
        add(html.slice(cursor, match.index))(match[1],true);
        cursor = match.index + match[0].length; }
    add(html.substr(cursor, html.length - cursor));
    code += 'return r.join("");';
    return new Function(code.replace(/[\r\t\n]/g, '')).apply(data); }

function reload(file, name2) { var name = name2==null?file:name2;
    var slot = document.getElementById(name);
    if (slot == null) return;
    slot.parentNode.replaceChild(svg(localStorage.getItem(file)),slot);}

function reload_cont(cont,name,element) { reload(name,element); if (null != cont) (cont)();   }

function loadFile(name,cont,element) {
    if (localStorage.getItem(name) == null) {
        var client = new XMLHttpRequest();
        client.open('GET', name, true);
        client.onload = function() {
            localStorage.setItem(name,client.responseText); 
            reload_cont(cont,name,element);
        }
        client.send(); }
    else reload_cont(cont,name,element); }

function svg(html) {
    return new DOMParser().parseFromString(html, "text/xml").firstChild;
}

function svgBulk() {
    var res = [];
    var node =  new DOMParser().parseFromString(html, "text/xml").firstChild;
    for (var i=0;i<node.childnode.length;i++) { var child = childNodes.item(i);
        if (child.tagName != undefined) res.push(child); } }

// The Card

//var color = ['#CE290F','#3B5998','#48AF5E','#FFEC00'];
var color = ['#DE3F26','#606060','#48AF5E','#FFC800'];
var slotName = slotNameDef;
function slotNameDef(x,y) { return "Slot-"+y+","+x; }

function card(line,pos,col,v) {
    return template_engine(
        localStorage.getItem("templates/Card.svg"),
            {   name: slotName(pos,line),
                suit: color[col-1],
                value: v,
                y: (line-1)*69,
                x: (pos-1)*42 }); }

// Game Scene

function setPlayerName(e, playerName) {
    var dx = (document.getElementById(e).attributes['fill'].value == "#FFFFFF") ? 65 : 20;
    document.getElementById(e).setAttribute("y",27);
    document.getElementById(e).setAttribute("x",dx);
    document.getElementById(e).textContent = playerName;
//    console.log("Bounding Box: "  + document.getElementById(e).getBBox().width);
    document.getElementById(e+"-Pad").setAttribute('width',
        document.getElementById(e).getBBox().width + 45); }

function place_card(x,y,c,v) { 
         var slot = document.getElementById(slotName(x,y));
         slot.parentNode.replaceChild(svg(card(y,x,c,v)),slot); }

function empty_card(x,y) { var slot = document.getElementById(slotName(x,y));
         var html = '<g xmlns="http://www.w3.org/2000/svg" id="'+slotName(x,y)+'"/>';
         slot.parentNode.replaceChild(svg(html),slot); }

function rand(lo,hi) { return Math.floor((Math.random()*hi)+lo); }

function loadScene() {
    reload("Kakaranet-Scene.svg", "Refined");
    //drawSampleCards();
     }

function findPlace() {
    for (var y=1;y<3;y++) for (var x=1;x<15;x++) {
        var e = document.getElementById(slotName(x,y));
        if (e.childElementCount == 0) {
            var pos = e.attributes['id'].value.split("-")[1].split(",");
            console.log("Free");
            console.log(pos[0]);
            console.log(pos[1]);
            return { y: pos[0], x: pos[1] }; } } }

function findCardOnTable(c,v) {
    console.log("Find Card " + c + " " + v);
    for (var y=1;y<3;y++) for (var x=1;x<15;x++) {
        var e = document.getElementById(slotName(x,y));
        if (e.childElementCount > 0) {
            var value = e.lastChild.textContent;
            var col = color.indexOf(e.lastChild.attributes['fill'].value) + 1;
            console.log(col + " " + value);
            if (c == col && v == value) return { 'x': x, 'y': y }; } } 
    console.log("Card Not Found");
    return ""; }


function loadAppend(file, animation, name) { 
    loadFile(file, function() {
        var slot = document.getElementById(name);
        var r = template_engine(localStorage.getItem(file),{'name': animation});
        document.getElementById(name).appendChild(svg(r)); }); }

function loadAnimationForButton(a, b) { return loadAppend('templates/ButtonAnimation.svg', a, b); }

// TODO: Rollout Monadic Chain here

loadFile('templates/Card.svg', function() { 
    loadFile('Kakaranet-Scene.svg', function() {

        var a = [{button: "Create", pathes: ["CreateShow", "CreateHide"]},
                 {button: "Play",   pathes: ["PlayShow",   "PlayHide"]}];

        for (var y=0;y<a.length;y++) for (var x=0;x<a[y].pathes.length;x++)
            loadAnimationForButton(a[y].pathes[x],a[y].button);

        document.getElementById("Right-Menu") .setAttribute('onclick', 'onRightMenu(evt)');
        document.getElementById("Play")       .setAttribute('onclick', 'onRightMenuDown(evt)');
        document.getElementById("Create")     .setAttribute('onclick', 'onRightMenuDown(evt)');
        document.getElementById("Point-Table").setAttribute('onclick', 'onPlayerInfo(evt)');
        document.getElementById("Player-Statistics").setAttribute('onclick', 'onPlayerInfoClose(evt)');

        for (var i=1;i<16;i++) { empty_card(i,2); empty_card(i,1); }

        document.getElementById('Page-1').addEventListener("mousewheel", mouseWheelHandler, false);

        // Setup Clipping ViewPorts

        var clipPath1 = svg('<clipPath id="myClip1"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path-Left" x="0" y="0" width="216" height="400"/></clipPath>');
        var clipPath2 = svg('<clipPath id="myClip2"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path-Right" x="0" y="0" width="216" height="400"/></clipPath>');
        var clipPath3 = svg('<clipPath id="myClip3"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path-Left-Chat" x="0" y="0" width="216" height="400"/></clipPath>');
        document.getElementsByTagName('defs').item(0).appendChild(clipPath1);
        document.getElementsByTagName('defs').item(0).appendChild(clipPath2);
        document.getElementsByTagName('defs').item(0).appendChild(clipPath3);
        document.getElementById("Online-List").setAttribute("clip-path","url(#myClip1)");
        document.getElementById("Chat").setAttribute("clip-path","url(#myClip2)");
        document.getElementById("Online-Chat").setAttribute("clip-path","url(#myClip1)");
        document.getElementById("Clip-Path-Left").setAttribute("transform", "translate(0,0)");
        document.getElementById("Clip-Path-Right").setAttribute("transform", "translate(0,0)");
        document.getElementById("Clip-Path-Left-Chat").setAttribute("transform", "translate(0,0)");

        document.getElementById('Player-Statistics').style.display = 'none';
        document.getElementById("Right-Bar").setAttribute("fill","skyblue");
        document.getElementById("Right-Bar").setAttribute("xmlns:data","Right-Bar");

        document.getElementById("Right-Bar").onmouseover = barHover;
        document.getElementById("Right-Bar").onmouseout = barHoverOut;
        document.getElementById("Left-Bar").onmouseover = onlineHover;
        document.getElementById("Left-Bar").onmouseout = onlineHoverOut;

        // HTML editors

        document.getElementById('onlineChatEdit').setAttribute("contentEditable","true");
        document.getElementById('onlineChatEdit').onkeydown = chatEditor;
        document.getElementById("onlineChatEdit").style.display = 'none';

        document.getElementById('edit').setAttribute("contentEditable","true");
        document.getElementById('edit').onkeydown = chatEditor;
        document.getElementById('edit').setAttribute("xmlns:data","Chat");
        document.getElementById("edit").style.display = '';

        // showOnlineList ctor

        var onlineListOnClick = ["Online-Users","Online-Users-Pad","Online-Logo",
            "722","723","users-online","users-online-text"];
        onlineListOnClick.map(
            function(x) { document.getElementById(x).onclick = showOnlineList; });

        onRightMenuDown();



    }, "Refined");
});



var removeChilds = function (e) { var last; while (last = e.lastChild) e.removeChild(last); };

function onPlayerInfo(evt) {
    ws.send(enc(tuple(atom('client'),
        tuple(atom('stats_action'),bin(document.user),atom('game_okey')))));
    }

function onPlayerInfoClose(evt) {
    document.getElementById('Player-Statistics').style.display = 'none';
}

function onRightMenu(evt) {
    localStorage.clear();
    ["PlayShow","CreateShow"].map(function (x) {
        document.getElementById(x+"-Motion").beginElement(); }); }

function onRightMenuDown(evt) {
    ["PlayHide","CreateHide"].map(function (x) {
        document.getElementById(x+"-Motion").beginElement(); }); }

function slotName1(x,y) { return "1Slot-"+y+","+x; }

function drawSampleCards() {
    slotName = slotName1;
    for (var i=1;i<15;i++) { place_card(i,rand(1,2),rand(1,4),rand(1,13)); }
    slotName = slotNameDef; }

var scrollSensitivity = 0.2;
var scroll_left = 5;
var scroll_left_chat = 5;
var scroll_right = -10000;

function chatMessage(chatName, id, me, string) {
    var i=0;
    var colors=['#FDFDFD','#DFF1F4'];
    var x1 = 7;
    var y1 = 0;
    var container = chatName == "Chat" ? "Right-Bar" : "Left-Bar";
    var hover = chatName == "Chat" ? "barHover" : "onlineHover";
    var translate_y = parseFloat(document.getElementById(chatName).getBBox().height);
    var x2 = 205;
    var textElement = chatText(container,id,me,string);
    var dy = translate_y == 0 ? 0 : translate_y + 10;
    var html = "<g xmlns='http://www.w3.org/2000/svg' " + 
        "id='Message-"+id+"' transform='translate(0,"+dy+")'></g>";
    var messageElement = svg(html);
    messageElement.appendChild(textElement);
    document.getElementById(chatName).appendChild(messageElement);
    create_multiline(textElement);
    var y2 = textElement.getBBox().height + 5;
    var box = "<path xmlns:data='"+container+"' xmlns='http://www.w3.org/2000/svg' d='M"+x1+","+y1+
                " L"+x2+","+y1+
            ((me == "Maxim") ?
                (" L"+x2+","+parseFloat(y2-7)+
                " L"+parseFloat(x2+7)+","+y2+
                " L"+x1+","+y2)
            :
                (" L"+x2+","+y2+
                " L"+0+","+y2+
                " L"+x1+","+parseFloat(y2-7)))
        + " L"+x1+","+y1+"' fill='"+colors[me=="Maxim"?1:0]+"'></path>";
    var boxElement = svg(box);
    textElement.setAttribute("xmlns:data",container)
    messageElement.insertBefore(boxElement,textElement);
    boxElement.setAttribute("mouseover",hover+"(evt);");
    boxElement.setAttribute("mouseout",hover+"Out(evt);");
    textElement.setAttribute("mouseover",hover+"(evt);");
    textElement.setAttribute("mouseout",hover+"Out(evt);");
    messageElement.setAttribute("onmouseover",hover+"(evt);");
    messageElement.setAttribute("onmouseout",hover+"Out(evt);");
//    console.log(messageElement);
}

function chatText(container, id, me, string) {
    var i = 0;
    var colors=['#3B5998'];
    var html = "<text xmlns:data='"+container+"' id='ChatText-"+id+"' width='180' " +
        " xmlns='http://www.w3.org/2000/svg' "+
        " font-family='Exo 2' font-size='16' font-weight='normal' fill='"+colors[i]+"'>" +
            string + "</text>";
//    console.log(html);
    return svg(html);
}

function mouseWheelHandler(e) {

    var leftBar = document.getElementById("Left-Bar");
    var rightBar = document.getElementById("Right-Bar");
    var leftFill = leftBar.getAttribute("fill");
    var rightFill = rightBar.getAttribute("fill");
    var leftActive = leftFill == "skyblue";
    var rightActive = rightFill == "skyblue";
    if (!leftActive && !rightActive) return;
//    console.log(leftActive);

    var evt = e;
    var scroll_dy = evt.detail ? evt.detail * scrollSensitivity : evt.wheelDelta * scrollSensitivity;
    var ori = leftActive ? (currentChat == null ? scroll_left : scroll_left ) : scroll_right;
    var scroll = parseFloat(scroll_dy) + parseFloat(ori);
    var selectedBar = leftActive ? (currentChat == null ? "Online-List" : currentChat) : "Chat";
    var selectedClip = leftActive ? (currentChat == null ? "Clip-Path-Left" : "Clip-Path-Left-Chat") : "Clip-Path-Right";
    var selectedBarShift = leftActive ? 2 : 857;
    var limit = parseFloat(document.getElementById(selectedBar).getBBox().height) - 400;
    if (scroll > 5) scroll = 5;
    if (scroll < -limit) scroll = -limit;
    document.getElementById(selectedClip).setAttribute("transform", "translate(0,"+parseFloat(-scroll)+")");
    document.getElementById(selectedBar).setAttribute("transform", "translate("+selectedBarShift+","+(parseFloat(95+scroll))+")");
    if (leftActive) scroll_left = scroll; else scroll_right = scroll;
    return true; 
}

var svgNS = "http://www.w3.org/2000/svg";

function create_multiline(target) {
    var text_element = target; // evt.target;
    var width = 190; //target.getAttribute("width");
    var words = text_element.firstChild.data.split('');
//    console.log(words);
//    var words = [].concat.apply([],lines.map(function(line) { return line.split(' '); }));;
    var start_x = 15; //text_element.getAttribute('x');
    text_element.firstChild.data = '';

    var tspan_element = document.createElementNS(svgNS, "tspan");
    tspan_element.setAttribute("x", start_x);
    tspan_element.setAttribute("xmlns:data", text_element.getAttribute("xmlns:data"));
    tspan_element.setAttribute("dy", 18);
    var text_node = document.createTextNode(words[0]);

    tspan_element.appendChild(text_node);
    text_element.appendChild(tspan_element);

    for(var i=1; i<words.length; i++) {
        if (words[i]=="") continue; 
        var len = tspan_element.firstChild.data.length;
        tspan_element.firstChild.data += words[i];

        if (tspan_element.getComputedTextLength() > width || words[i]=="\n") {
            if (words[i]=='\n') words[i]="";
            tspan_element.firstChild.data = tspan_element.firstChild.data.slice(0, len);
            var tspan_element = document.createElementNS(svgNS, "tspan");
            tspan_element.setAttribute("x", start_x);
            tspan_element.setAttribute("xmlns:data", text_element.getAttribute("xmlns:data"));
            tspan_element.setAttribute("dy", 18);
            text_node = document.createTextNode(words[i]);
            tspan_element.appendChild(text_node);
            text_element.appendChild(tspan_element);
        }
    }
}

function barHover(evt) { document.getElementById("Right-Bar").setAttribute("fill","skyblue"); }
function barHoverOut(evt) { document.getElementById("Right-Bar").setAttribute("fill","lightblue"); }
function onlineHover(evt) { document.getElementById("Left-Bar").setAttribute("fill","skyblue"); }
function onlineHoverOut(evt) { document.getElementById("Left-Bar").setAttribute("fill","lightblue"); }
function onlineHoverColor(evt) {
    onlineHover(evt);
    var name = evt.target.getAttribute("xmlns:data");
    if (null != name) document.getElementById(name).setAttribute("fill","#FFF687");
}
function onlineHoverOutColor(evt) { 
    onlineHoverOut(evt);
    var name = evt.target.getAttribute("xmlns:data");
    if (null != name) document.getElementById(name).setAttribute("fill","#DBEBED");
}

function chatEditor(evt) {
    var chatContainer = evt.target.getAttribute("xmlns:data");
    if (evt.keyCode == 13 && evt.metaKey == false) {
        var e = evt.target; //document.getElementById('edit');
        console.log(e);
        if (e.innerText.trim() != ""){
            var text = e.innerText.trim().encodeHTML();
            chatMessage(chatContainer,"100","Maxim",text);
//            if (null != currentChat)
                ws.send(enc(tuple(atom('client'),
                    tuple(atom('message'),bin(document.user),bin(chatContainer.substr(5)),bin(text)))));
            e.innerHTML = '';
        }
    } else if (evt.keyCode == 13 && evt.metaKey == true) {
        document.execCommand('insertText',false, '\n');
    }
    var scroll = -1000000;
    if (null != currentChat) left_scroll = scroll;
    mouseWheelHandler({'detail':scroll,'wheelDelta':scroll});
}

function shiftTranslate(name,shiftValue) {
    var rect = document.getElementById(name);
    if (null == rect) return;
    var remove = rect.parentNode.parentNode;
    var children = document.getElementById("Online-List").childNodes;
    var removeIndex = -1;
    for(var i = 0; i<children.length; ++i) { 
        var child = children[i];
        if (child == remove) removeIndex = i;
        if (removeIndex != -1 && i>=removeIndex)
            child.setAttribute("transform","translate(0,"+(parseFloat(i)+parseFloat(shiftValue))
                *remove.getBBox().height+")");
    }
    return rect.parentNode.parentNode;
}

function removeOnlineUser(name) {
    shiftTranslate(name,-2).remove();
}

function openChat(evt) {
    document.getElementById("Online-List").style.display = 'none';
    document.getElementById("onlineChatEdit").style.display = '';
    var name = evt.target.getAttribute("xmlns:data");
    currentChat = "Chat+"+name;
    var chatElement = document.getElementById(currentChat);
    if (null == chatElement) {
        // read from local KVS
        var html = '<g xmlns="http://www.w3.org/2000/svg" id="'+currentChat+'" y="0" clip-path="url(#myClip3)" transform="translate(1.000000, 107.000000)"></g>';
        document.getElementById("Page-1").appendChild(svg(html));
        chatMessage(currentChat,"1","System","You can chat with\n"+name);
    } else {
        document.getElementById(currentChat).style.display = '';
    }
    document.getElementById("onlineChatEdit").setAttribute("xmlns:data",currentChat);
    scroll_left = -1000000;

    onlineHover();
    mouseWheelHandler({'detail':-100000,'wheelDelta':-100000});
    onlineHoverOut();

//    document.getElementById("users-online-text").textContent = currentChat;
}

function showOnlineList(evt) {


    document.getElementById("onlineChatEdit").style.display = 'none';
    if (null != currentChat) {
        document.getElementById(currentChat).style.display = 'none';
    }
    document.getElementById("Online-List").style.display = '';
    currentChat = null;

    scroll_left = 0;
    onlineHover();
    mouseWheelHandler({'detail':scroll_left,'wheelDelta':scroll_left});
    onlineHoverOut();

}

var currentChat = null;

function editorControl(id,left) {
    var x = left == "left" ? 0 : 864;
    var events = ' onmouseover="barHover(evt);" onmouseout="barHoverOut(evt)" ';
    var html = '<foreignObject  xmlns="http://www.w3.org/2000/svg" x="'+x+'" y="504" width="198" height="120" ' + events + '>' +
        '<div id="'+id+'" style="padding:4px;background-color:#FFF687;color:#3B5998;'+
        'font-family:"Exo 2";font-size:16px;contentEditable="true" '+
        'xmlns="http://www.w3.org/1999/xhtml">Write here some text.</div></foreignObject>';
    var element = svg(html);
    return element;
}

function addOnlineUser(name,full_name,insertMode) {
    var listElement = document.getElementById("Online-List");
    var clickEvent = ' onclick="openChat(evt);" '; 
    var events = ' onmouseover="onlineHover(evt);" onmouseout="onlineHoverOut(evt);" ' + clickEvent;
    var eventsColor = ' onmouseover="onlineHoverColor(evt);" onmouseout="onlineHoverOutColor(evt);" ' + clickEvent;
    var color = insertMode == "insertTop" ? "red" : "green";
    var y = (insertMode == "insertTop") ? "0" : listElement.getBBox().height;
    var html = '<g xmlns="http://www.w3.org/2000/svg" height="60" transform="translate(0, '+y+')">' +
            '<g xmlns:data="'+name+'" fill="#DBEBED" '+eventsColor+'>' +
            '    <rect cursor="pointer" xmlns:data="'+name+'" fill="#DBEBED" id="'+name+'" x="10" y="0" width="196" height="48" ' +'></rect></g>' +
            '<text xmlns:data="'+name+'" '+eventsColor+' '+
            'font-family="Exo 2" font-size="18" cursor="pointer" font-weight="normal" line-spacing="18"'+
            ' fill="#3B5998">' +
                '<tspan xmlns:data="'+name+'" font-weight="normal" fill="'+color+'" x="19" y="22">'+full_name+'</tspan>' + 
                '<tspan xmlns:data="'+name+'" font-size="14" x="19" y="40">Score: 1043 Pos: 13</tspan></text>'+
            '<rect '+
            '  x="10" y="48" width="196" height="8"></rect></g>';
    var element = svg(html);
    if (insertMode == "insertTop") {
        var firstElement = listElement.firstElementChild;
        var first = firstElement.firstElementChild.getAttribute("xmlns:data");
        shiftTranslate(first,0);
        listElement.insertBefore(element,firstElement);
    } else listElement.appendChild(element);
}

        chatMessage("Chat","1","Maxim2","Joe:\nHello There!".encodeHTML());
        chatMessage("Chat","2","Maxim2","Alice:\nYou got new Design. Eh?".encodeHTML());
        chatMessage("Chat","3","Maxim","Maxim So:\nThis was made with pure SVG".encodeHTML());

        barHover();
        mouseWheelHandler({'detail':-100000,'wheelDelta':-100000});
        barHoverOut();

