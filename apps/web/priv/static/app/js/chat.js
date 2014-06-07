
// Online User Chat and In-Game Chat

var scrollSensitivity = 0.2;
var scroll_left = 5;
var scroll_left_chat = 5;
var scroll_right = -10000;
var currentChat = null;

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
    document.getElementById("Online-List").style.display = '';
    currentChat = null;

    scroll_left = 0;
    onlineHover();
    mouseWheelHandler({'detail':scroll_left,'wheelDelta':scroll_left});
    onlineHoverOut();
}

