locale = { en: {}, tr: {} };

// Buttons and Interface

locale.tr.Score       = "Puan";
locale.tr.Online      = "Oynayanlar";
locale.tr.Games       = "Oyunlar";
locale.tr.Reveals     = "Bitirdi";
locale.tr.Rules       = "Kurallar";
locale.tr.Quota       = "Kota";
locale.tr.Kakush      = "Kakuş";
locale.tr.Login       = "İle bağlan";
locale.tr.Statistics  = "İstatistikler";

// Chats

locale.en.EditMessage = "Write some text here";
locale.tr.EditMessage = "Chat için buraya yazabilirsiniz";
locale.en.GameChat    = "This is In-Game chat";
locale.tr.GameChat    = "Oyun içi sohbet";
locale.en.PrivateChat = " is waiting for chat with you";
locale.tr.PrivateChat = " seninle sohbet için bekliyor";

// Messages

locale.tr.Paused      = "ara istiyor";
locale.en.Paused      = "paused";
locale.tr.Revealed    = "bitirdi";
locale.en.Revealed    = "revealed";
locale.en.WrongReveal = "Wrong Reveal. Try next time.";
locale.tr.WrongReveal = "Bu el yanlış. Bitirde gel";

// Rules Dialog

locale.en.FakeJoker   = "Fake Joker";
locale.tr.FakeJoker   = "Sahte okey";
locale.tr.WinningDeck = "Açan el";
locale.en.WinningDeck = "Winning Deck";
locale.tr.OkeyRules   = "Oyun kuralları";
locale.en.OkeyRules   = "Okey Rules";
locale.tr.Run         = "Sıralı";
locale.tr.Set         = "Asker";
locale.tr.Pair        = "Çift";

// Protocol

locale.tr.player_left         = "Oyundan kaçış";
locale.tr.okey_revealed       = "Açtığı el";
locale.tr.okey_round_ended    = "Tamamladığı oyun";
locale.tr.okey_turn_timeout   = "Zamanında oynamama";
locale.tr.wrong_reveal        = "Yanlış açma";
locale.tr.okey_game_started   = "Oynadığı oyun";

function i18n(word)
{
    var translation = locale[currentLanguage()][word];
    return (translation == null) ? word : translation;
}

function currentLanguage()
{
    var currentLocale = localStorage.getItem("locale");
    return currentLocale == null ? "en" : currentLocale;
}

function switchLanguage()
{
    if (localStorage.getItem("locale") == "tr")
    {
        $("#Flag-tr").hide();
        $("#Flag-en").show();
        localStorage.setItem("locale","en");
    } else {
        $("#Flag-en").hide();
        $("#Flag-tr").show();
        localStorage.setItem("locale","tr");
    }
}

function translateScene(e)
{
    if (document.getElementById("Okey-Rules") != null) {
        $("#Okey-Rules-Text")[0].lastElementChild.textContent = i18n("OkeyRules");
        $("#Pair")[0].lastElementChild.textContent = i18n("Pair");
        $("#Set")[0].lastElementChild.textContent = i18n("Set");
        $("#Run")[0].lastElementChild.textContent = i18n("Run");
        $("#Winning-Deck")[0].lastElementChild.textContent = i18n("WinningDeck");
    if (currentLanguage() == "en") { $("#Turkish-Rules").hide(); $("#English-Rules").show(); } 
                              else { $("#Turkish-Rules").show(); $("#English-Rules").hide(); }
    }

    if (document.getElementById("Player-Statistics") != null) {
        $("#Games-Text")[0].lastElementChild.textContent = i18n("Games");
        $("#Reveals-Text")[0].lastElementChild.textContent = i18n("Reveals");
    if (currentLanguage() == "en") { $("#Turkish-StatNotes").hide(); $("#English-StatNotes").show(); } 
                              else { $("#Turkish-StatNotes").show(); $("#English-StatNotes").hide(); }
    }

    try {
    document.getElementById("GameChatEditor").firstElementChild.textContent = i18n("EditMessage");
    document.getElementById("OnlineChatEditor").firstElementChild.textContent = i18n("EditMessage");
    } catch (e) { console.log("Please add foreignObjects to schene from SVG.txt"); }



    $("#Users-Online-Message")[0].firstElementChild.textContent = i18n("Online");
    $("#Point-Table").find("text")[0].lastElementChild.textContent = i18n("Statistics");
    $("#Rules").find("text")[0].lastElementChild.textContent = i18n("Rules");
    $("#Kakush")[0].lastElementChild.textContent = i18n("Kakush") + ": " + 0;
    $("#Gabrielo-Discard-Shape").hide();
    $("#Center-Card-Selection").hide();
    $("#You-Discard-Shape").hide();

    $('#Facebook-Login').on('click',function(x) { fb_login(); });
    $('#Facebook-Login').attr({cursor:'pointer'});
    $('#Login-Text')[0].style.cursor = 'pointer';
//    $("#Login-Text")[0].lastElementChild.textContent = i18n("Login");
    $("#Login-Text").text(i18n("Login")).attr({x:16,y:23});

//    $("#Okey").hide();
    $("#Okey").on("click", sendSawOkey);
//    $("#Have-8-Tashes").hide();

}
