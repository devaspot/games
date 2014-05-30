
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

function handle_web_socket(body) {
//    console.log(dec(body).value[0][0].value);
    switch (dec(body).value[0][0].value) {
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
        default: console.log(String(dec(body)));
    }
}

// SVG DOM and Template Engine

function template_engine(html, data) {
    var re = /{([^}]+)?}/g, code = 'var r=[];', cursor = 0;
    var add = function(line,js) {
        js? (code += 'r.push(' + line + ');') :
            (code += line != '' ? 'r.push("' + line.replace(/"/g, '\\"') + '");' : '');
        return add; }
    while(match = re.exec(html)) {
        add(html.slice(cursor, match.index))(match[1],true);
        cursor = match.index + match[0].length; }
    add(html.substr(cursor, html.length - cursor));
    code += 'return r.join("");';
    return new Function(code.replace(/[\r\t\n]/g, '')).apply(data); }

function reload(file, name) { var slot = document.getElementById(name);
         slot.parentNode.replaceChild(svg(localStorage.getItem(file)),slot);}

function reload_cont(cont,name,element) { if (null != cont) (cont)(); else reload(name,element); }

function loadFile(name,cont,element) {
    if (localStorage.getItem(name) == null) {
        var client = new XMLHttpRequest();
        client.open('GET', name, true);
        client.onload = function() {
            localStorage.setItem(name,client.responseText); 
            reload_cont(cont,name,element); }
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
    for (var i=1;i<16;i++) { empty_card(i,2); empty_card(i,1); }
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

        loadScene();

        var a = [{button: "Create", pathes: ["CreateShow", "CreateHide"]},
                 {button: "Play",   pathes: ["PlayShow",   "PlayHide"]}];

        for (var y=0;y<a.length;y++) for (var x=0;x<a[y].pathes.length;x++)
            loadAnimationForButton(a[y].pathes[x],a[y].button);

        document.getElementById("Right-Menu") .setAttribute('onclick', 'onRightMenu(evt)');
        document.getElementById("Play")       .setAttribute('onclick', 'onRightMenuDown(evt)');
        document.getElementById("Create")     .setAttribute('onclick', 'onRightMenuDown(evt)');
        document.getElementById("Point-Table").setAttribute('onclick', 'onPlayerInfo(evt)');
        document.getElementById("Player-Statistics").setAttribute('onclick', 'onPlayerInfoClose(evt)');
        
        document.getElementById('Page-1').addEventListener("mousewheel", mouseWheelHandler, false);
        
        var clipPath = svg('<clipPath id="myClip"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path" x="0" y="0" width="216" height="400"/></clipPath>');
        document.getElementsByTagName('defs').item(0).appendChild(clipPath);
        document.getElementById("Online-List").setAttribute("clip-path","url(#myClip)");
        document.getElementById("Chat").setAttribute("clip-path","url(#myClip)");
        document.getElementById("Clip-Path").setAttribute("transform", "translate(0,0)");
        document.getElementById('Player-Statistics').style.display = 'none';
        document.getElementById("Right-Bar").onmouseover = barHover;
        document.getElementById("Right-Bar").onmouseout = barHoverOut;
        document.getElementById('edit').setAttribute("contentEditable","true");
        document.getElementById('edit').onkeydown = chatEditor;
        
        
        
//        onRightMenuDown();

    });
});

var removeChilds = function (node) {
    var last;
    while (last = node.lastChild) node.removeChild(last);
};

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

// SVG Samples for svg.htm

/*
loadFile('templates/Mustafa-Persona.svg',   null, "Mustafa-Persona-Sample");
loadFile('templates/Mustafa-Selection.svg', null, "Mustafa-Selection-Sample");
document.getElementById("MustafaSelection").addEventListener('click', function() {
    var style = document.getElementById("Mustafa-Selection-Sample").style;
    if (style.display == 'none') style.display = 'block';
                            else style.display = 'none'; });
*/

function slotName1(x,y) { return "1Slot-"+y+","+x; }

function drawSampleCards() {
    slotName = slotName1;
    for (var i=1;i<15;i++) { place_card(i,rand(1,2),rand(1,4),rand(1,13)); }
    slotName = slotNameDef; }

var scrollSensitivity = 0.2;
var scroll = 0.0;

function chatMessage(id, me, string) {
    var i=0;
    var colors=['white','#DFF1F4'];
    var x1 = 7;
    var y1 = 0;
    var translate_y = parseFloat(document.getElementById("Chat").getBBox().height);
    var x2 = 205;
    var textElement = chatText(id,me,string);
    var dy = translate_y == 0 ? 0 : translate_y + 10;
    var html = "<g xmlns='http://www.w3.org/2000/svg' " + 
        "id='Message-"+id+"' transform='translate(0,"+dy+")'></g>";
    var messageElement = svg(html);
    messageElement.appendChild(textElement);
    document.getElementById("Chat").appendChild(messageElement);
    create_multiline(textElement);
    var y2 = textElement.getBBox().height + 5;
    var box = "<path xmlns='http://www.w3.org/2000/svg' d='M"+x1+","+y1+
                " L"+x2+","+y1+
            ((string == "Maxim") ?
                (" L"+x2+","+parseFloat(y2-7)+
                " L"+parseFloat(x2+7)+","+y2+
                " L"+x1+","+y2)
            :
                (" L"+x2+","+y2+
                " L"+0+","+y2+
                " L"+x1+","+parseFloat(y2-7)))
        + " L"+x1+","+y1+"' fill='"+colors[string=="Maxim"?1:0]+"'></path>";
    var boxElement = svg(box);
    messageElement.insertBefore(boxElement,textElement);
    boxElement.setAttribute("mouseover","barHover(evt);");
    boxElement.setAttribute("mouseout","barHoverOut(evt);");
    textElement.setAttribute("mouseover","barHover(evt);");
    textElement.setAttribute("mouseout","barHoverOut(evt);");
    messageElement.setAttribute("onmouseover","barHover(evt);");
    messageElement.setAttribute("onmouseout","barHoverOut(evt);");
    console.log(messageElement);
}

function chatText(id, me, string) {
    var i = 0;
    var colors=['#3B5998'];
    var html = "<text id='ChatText-"+id+"' width='180' " +
        " xmlns='http://www.w3.org/2000/svg' "+
        " font-family='Exo 2' font-size='16' font-weight='normal' fill='"+colors[i]+"'>" +
            string + "</text>";
    console.log(html);
    return svg(html);
}

function mouseWheelHandler(e) {
    var evt = e;
    var scroll_dy = evt.detail ? evt.detail * scrollSensitivity : evt.wheelDelta * scrollSensitivity;
    var ori = scroll;
    scroll = parseFloat(scroll_dy) + parseFloat(ori);
    var limit = parseFloat(document.getElementById("Chat").getBBox().height) - 400;
    if (scroll > 5) scroll = 5;
    if (scroll < -limit) scroll = -limit;
    document.getElementById("Clip-Path").setAttribute("transform", "translate(0,"+parseFloat(-scroll)+")");
    document.getElementById("Online-List").setAttribute("transform", "translate(0,"+(parseFloat(95+scroll))+")");
    document.getElementById("Chat").setAttribute("transform", "translate(857,"+(parseFloat(95+scroll))+")");
    return true; 
}

var svgNS = "http://www.w3.org/2000/svg";

function create_multiline(target) {
    var text_element = target; // evt.target;
    var width = target.getAttribute("width");
    var words = text_element.firstChild.data.split('');
    console.log(words);
//    var words = [].concat.apply([],lines.map(function(line) { return line.split(' '); }));;
    var start_x = 15; //text_element.getAttribute('x');
    text_element.firstChild.data = '';

    var tspan_element = document.createElementNS(svgNS, "tspan");
    tspan_element.setAttribute("x", start_x);
    tspan_element.setAttribute("dy", 18);
    var text_node = document.createTextNode(words[0]);

    tspan_element.appendChild(text_node);
    text_element.appendChild(tspan_element);

    for(var i=1; i<words.length; i++) {
        if (words[i]=="") continue; 
        var len = tspan_element.firstChild.data.length;
        tspan_element.firstChild.data += words[i];

        if (tspan_element.getComputedTextLength() > width || words[i]=="\n") {
            words[i]="";
            tspan_element.firstChild.data = tspan_element.firstChild.data.slice(0, len);
            var tspan_element = document.createElementNS(svgNS, "tspan");
            tspan_element.setAttribute("x", start_x);
            tspan_element.setAttribute("dy", 18);
            text_node = document.createTextNode(words[i]);
            tspan_element.appendChild(text_node);
            text_element.appendChild(tspan_element);
        }
    }
}

function barHover(evt) {
    var target = document.getElementById("Right-Bar");
    target.setAttribute("fill","#9CADCB");
}

function barHoverOut(evt) {
    var target = document.getElementById("Right-Bar");
    target.setAttribute("fill","lightblue");
}

function chatEditor(evt) {
    if (evt.keyCode == 13 && evt.metaKey == false) {
        var e = document.getElementById('edit');
        if (e.innerText.trim() != ""){
            chatMessage("100","Maxim",e.innerText.trim().encodeHTML());
            e.innerHTML = '';
        }
    } else if (evt.keyCode == 13 && evt.metaKey == true) {
        document.execCommand('insertText',false, '\n');
    }
    var scroll = -10000000;
    mouseWheelHandler({'detail':scroll,'wheelDelta':scroll});
}

if (!String.prototype.encodeHTML) {
  String.prototype.encodeHTML = function () {
    return this.replace(/&/g, '&amp;')
               .replace(/</g, '&lt;')
               .replace(/>/g, '&gt;')
               .replace(/"/g, '&quot;')
               .replace(/'/g, '&apos;');
  };
}
