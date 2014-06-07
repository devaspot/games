
var svgNS = "http://www.w3.org/2000/svg";
var color = ['#DE3F26','#606060','#48AF5E','#FFC800'];
var slotName = slotNameDef;

function svg(html) { return new DOMParser().parseFromString(html, "text/xml").firstChild; }
function removeChilds(e) { var last; while (last = e.lastChild) e.removeChild(last); };
function slotNameDef(x,y) { return "Slot-"+y+","+x; }
function loadAnimationForButton(a, b) { return loadAppend('svg/ButtonAnimation.svg', a, b); }

function setPlayerName(e, playerName) {
    var dx = 15; //(document.getElementById(e+"-Name").attributes['fill'].value == "#FFFFFF") ? 65 : 20;
    document.getElementById(e+"-Name").setAttribute("y",27);
    document.getElementById(e+"-Name").setAttribute("x",dx);
    document.getElementById(e+"-Name").textContent = playerName;
    document.getElementById(e+"-Pad").setAttribute('width', document.getElementById(e).getBBox().width + 15); }

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

function loadAppend(file, animation, name) { 
    loadFile(file, function() {
        var slot = document.getElementById(name);
        var r = template_engine(localStorage.getItem(file),{'name': animation});
        document.getElementById(name).appendChild(svg(r)); }); }

function card(line,pos,col,v) {
    return template_engine(
        localStorage.getItem("svg/Card.svg"),
            {   name: slotName(pos,line),
                suit: color[col-1],
                value: v,
                y: (line-1)*69,
                x: (pos-1)*42 }); }

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
    document.getElementById("Right-Bar").setAttribute("fill","skyblue");
    document.getElementById("Right-Bar").setAttribute("xmlns:data","Right-Bar");

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
}

function onPlayerInfo(evt) {
    ws.send(enc(tuple(atom('client'),
        tuple(atom('stats_action'),bin(document.user),atom('game_okey'))))); }

function onPlayerInfoClose(evt) { document.getElementById('Player-Statistics').style.display = 'none'; }

// Run

loadFile('Kakaranet-Scene.svg', function() { PatchSVG(), StartApp();}, "Refined");
