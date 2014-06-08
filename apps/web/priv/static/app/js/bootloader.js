
var svgNS = "http://www.w3.org/2000/svg";
var color = ['#DE3F26','#606060','#48AF5E','#FFC800'];

function parseTransformAttribute(aa) {
    var a = aa.split(' ').join('');
    var b={};
    for (var i in a = a.match(/(\w+\((\-?\d+\.?\d*,?)+\))+/g)) { var c = a[i].match(/[\w\.\-]+/g); b[c.shift()] = c; }
    return b; }

function svg(html) { return new DOMParser().parseFromString(html, "text/xml").firstChild; }
function removeChilds(e) { var last; while (last = e.lastChild) e.removeChild(last); };

function setPlayerName(e, playerName) {
    var dx = 15;
    document.getElementById(e+"-Name").setAttribute("y",27);
    document.getElementById(e+"-Name").setAttribute("x",dx);
    document.getElementById(e+"-Name").textContent = playerName;
    document.getElementById(e+"-Pad").setAttribute('width', document.getElementById(e+"-Name").getBBox().width + 25); }

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

function discarder(name) { return template_engine(localStorage.getItem("svg/Discarder.svg?q=" + $.timestamp), { name: name }); }

function initDiscards() {
    [ {name:"Gabrielo-Discard", hand:"Player-Left-Hand"},
      {name:"Alina-Discard",    hand:"Player-Right-Hand"},
      {name:"Mustafa-Discard",  hand:"Player-Center-Hand"},
      {name:"You-Discard",      hand:"Player-Me-Hand"}                 ].map(function(e) {
//        document.getElementById(e.name).firstElementChild.remove();
        document.getElementById(e.name).appendChild(svg(discarder(e.hand)));
         });
}

function PatchSVG()
{
//    document.getElementById('Page-1').addEventListener("mousewheel", mouseWheelHandler, false);

        // Setup Clipping ViewPorts

    var clipPath1 = svg('<clipPath id="myClip1"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path-Left" x="0" y="0" width="216" height="400"/></clipPath>');
    var clipPath2 = svg('<clipPath id="myClip2"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path-Right" x="0" y="0" width="216" height="400"/></clipPath>');
    var clipPath3 = svg('<clipPath id="myClip3"><rect xmlns="http://www.w3.org/2000/svg" id="Clip-Path-Left-Chat" x="0" y="0" width="216" height="400"/></clipPath>');
    document.getElementsByTagName('defs').item(0).appendChild(clipPath1);
    document.getElementsByTagName('defs').item(0).appendChild(clipPath2);
    document.getElementsByTagName('defs').item(0).appendChild(clipPath3);

//     document.getElementById("Online-List").setAttribute("clip-path","url(#myClip1)");
//     document.getElementById("Chat").setAttribute("clip-path","url(#myClip2)");
//     document.getElementById("Online-Chat").setAttribute("clip-path","url(#myClip1)");

    document.getElementById("Clip-Path-Left").setAttribute("transform", "translate(0,0)");
    document.getElementById("Clip-Path-Right").setAttribute("transform", "translate(0,0)");
    document.getElementById("Clip-Path-Left-Chat").setAttribute("transform", "translate(0,0)");

//  document.getElementById('Player-Statistics').style.display = 'none';
    document.getElementById("Right-Bar").setAttribute("fill","lightblue");
    document.getElementById("Left-Bar").setAttribute("fill","lightblue");
    document.getElementById("Right-Bar").setAttribute("xmlns:data","Right-Bar");
    document.getElementById("Left-Bar").setAttribute("xmlns:data","Left-Bar");

    document.getElementById("Right-Bar").onmouseover = barHover;
    document.getElementById("Right-Bar").onmouseout = barHoverOut;
    document.getElementById("Left-Bar").onmouseover = onlineHover;
    document.getElementById("Left-Bar").onmouseout = onlineHoverOut;
    
    document.getElementById("Player-Left").style.display = 'block';


    // HTML editors

/*
    document.getElementById('onlineChatEdit').setAttribute("contentEditable","true");
    document.getElementById('onlineChatEdit').onkeydown = chatEditor;
    document.getElementById("onlineChatEdit").style.display = 'none';

    document.getElementById('edit').setAttribute("contentEditable","true");
    document.getElementById('edit').onkeydown = chatEditor;
    document.getElementById('edit').setAttribute("xmlns:data","Chat");
    document.getElementById("edit").style.display = '';
*/
    // showOnlineList ctor

    var onlineListOnClick = [
        "Online-Users",
        "Online-Users-Pad",
        "Online-Logo",
        "Users-Online-Message",
        "Users-Online-Number" ];

//       onlineListOnClick.map(function(x) { 
//           console.log(x);
//            document.getElementById(x).onclick = showOnlineList; });

    initDiscards();

    Core(ControllerScope);
    Core(DragScope);
    Core(DropScope);
    Core(TimerScope);
    Core(PlayerScope);
    Core(OkeyApiProviderScope);
    Core(CardScope);
    Core(HandScope);
    Core(DeckScope);

    $svg.attr({preserveAspectRatio:"xMidYMin meet",class:"svg"});
}

function onPlayerInfo(evt) {
    ws.send(enc(tuple(atom('client'),
        tuple(atom('stats_action'),bin(document.user),atom('game_okey'))))); }

function onPlayerInfoClose(evt) { document.getElementById('Player-Statistics').style.display = 'none'; }

// Run

$.load('Kakaranet-Scene.svg', function(x) { 
    var name = "Refined";
    var slot = document.getElementById(name);
    if (slot == null) return;
    slot.parentNode.replaceChild(svg(x),slot);
    $.load("svg/Discarder.svg", function(h) {
        PatchSVG();
        StartApp(); 
    });
});
