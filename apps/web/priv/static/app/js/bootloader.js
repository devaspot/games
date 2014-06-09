
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
        document.getElementById(e.name).appendChild(svg(discarder(e.hand)));
         });
}

function PatchSVG()
{

    $('Player-Statistics').hide();

    var onlineListOnClick = [
        "Online-Users",
        "Online-Users-Pad",
        "Online-Logo",
        "Users-Online-Message",
        "Users-Online-Number" ];

       onlineListOnClick.map(function(x) { 
            document.getElementById(x).onclick = showOnlineList; });

    Core(ControllerScope);
    Core(DragScope);
    Core(DropScope);
    Core(TimerScope);
    Core(PlayerScope);
    Core(OkeyApiProviderScope);
    Core(CardScope);
    Core(HandScope);
    Core(DeckScope);
    Core(RosterScope);

    initDiscards();
    initChat();
    initChatSample();

    //document.addEventListener('touchmove',function(e) {e.preventDefault();},false);
    $svg.attr({preserveAspectRatio:"xMidYMid meet",width:"100%",height:"100%"});
}

function initChatSample() {
    chatMessage("Chat","1","Maxim2","Joe:\nHello There!".encodeHTML());
    chatMessage("Chat","2","Maxim2","Alice:\nYou got new Design. Eh?".encodeHTML());
    chatMessage("Chat","3","Maxim","Maxim So:\nThis was made with pure SVG".encodeHTML());

    barHover();
    mouseWheelHandler({'detail':-100000,'wheelDelta':-100000});
    barHoverOut();
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
