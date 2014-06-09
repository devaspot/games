
// Online User Chat and In-Game Chat

var scrollSensitivity = 0.2;
var scroll_left = 5;
var scroll_left_chat = 5;
var scroll_right = -10000;
var currentChat = null;
var user_count = 0;

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

function showOnlineList(evt)
{
    document.getElementById("onlineChatEdit").style.display = 'none';
    if (null != currentChat) { document.getElementById(currentChat).style.display = 'none'; }
    document.getElementById("Online-List").style.display = 'block';
    currentChat = null;

    scroll_left = 0;
    onlineHover();
    mouseWheelHandler({'detail':scroll_left,'wheelDelta':scroll_left});
    onlineHoverOut();
}

if (!String.prototype.encodeHTML) {
  String.prototype.encodeHTML = function () {
    return this.replace(/&/g, '&amp;')
               .replace(/</g, '&lt;')
               .replace(/>/g, '&gt;')
               .replace(/"/g, '&quot;')
               .replace(/'/g, '&apos;'); // "
  };
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
        if (null == firstElement) listElement.appendChild(element); else {
            var first = firstElement.firstElementChild.getAttribute("xmlns:data");
            shiftTranslate(first,0);
            listElement.insertBefore(element,firstElement);
        }
    } else listElement.appendChild(element);
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
