
var transition = {pid: '', port: '8000' };

// BERT Protocol

function handle_web_socket(body) {
    console.log(dec(body).value[0][2].value);
    switch (dec(body).value[0][2].value) {
        case 'okey_game_started': 
            var a = dec(body).value[0][3][0].value[0][1];
            console.log("Started: " + a.length);
            for (var i=1;i<=a.length;i++) {
                var c = a[i-1].value[0][1];
                var v = a[i-1].value[0][2];
                console.log("Card " + c + " " + v);
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
                console.log(pos);
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

function svg(html) { return new DOMParser().parseFromString(html, "text/xml").firstChild; }

// The Card

var color = ['#CE290F','#3B5998','#48AF5E','#FFEC00'];
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

function place_card(x,y,c,v) { 
         var slot = document.getElementById(slotName(x,y));
         slot.parentNode.replaceChild(svg(card(y,x,c,v)),slot); }

function empty_card(x,y) { var slot = document.getElementById(slotName(x,y));
         var html = '<g xmlns="http://www.w3.org/2000/svg" id="'+slotName(x,y)+'"/>';
         slot.parentNode.replaceChild(svg(html),slot); }

function rand(lo,hi) { return Math.floor((Math.random()*hi)+lo); }

function loadScene() {
    reload("Kakaranet-7-Refined.svg", "Refined");
    for (var i=1;i<15;i++) { empty_card(i,2); empty_card(i,1); }
    drawSampleCards(); }

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

// Monadic Chain here

loadFile('templates/Card.svg', function() { 
  loadFile('Kakaranet-7-Refined.svg', loadScene); 
});

// SVG Samples for svg.htm

loadFile('templates/Mustafa-Persona.svg', null, "Mustafa-Persona-Sample");
loadFile('templates/Mustafa-Selection.svg', null, "Mustafa-Selection-Sample");

document.getElementById("MustafaSelection").addEventListener('click', function() { 
    var style = document.getElementById("Mustafa-Selection-Sample").style;
    if (style.display == 'none') style.display = 'block'; 
                            else style.display = 'none'; });

function slotName1(x,y) { return "1Slot-"+y+","+x; }

function drawSampleCards() {
    slotName = slotName1;
    for (var i=1;i<15;i++) { place_card(i,rand(1,2),rand(1,4),rand(1,13)); }
    slotName = slotNameDef; }
