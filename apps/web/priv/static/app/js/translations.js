locale = { en: {}, tr: {} };

// Buttons and Interface

locale.tr.Score       = "Puan";
locale.tr.Online      = "Oynayanlar";
locale.tr.Games       = "Oyunlar";
locale.tr.Rules       = "Kurallar";
locale.tr.Quota       = "Kota";
locale.tr.Kakush      = "Kakuş";
locale.tr.Login       = "Üye Ol";
locale.tr.Statistics  = "İstatistikler";

// Chats

locale.en.EditMessage = "Write some text here";
locale.tr.EditMessage = "Burada bazı metin yazmak";
locale.en.GameChat    = "This is In-Game chat";
locale.tr.GameChat    = "Oyun sohbet";
locale.en.PrivateChat = "is waiting for chat with you";
locale.tr.PrivateChat = "seninle sohbet için bekliyor";

// Messages

locale.tr.Paused      = "ara istiyor";
locale.en.Paused      = "paused";
locale.tr.Revealed    = "bitirdi";
locale.en.Paused      = "revealed";
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
locale.en.RulesDetails = "Make 3x3+1x5, 2x3+2x4 or 2x5+1x4 Runs/Sets or 7 Pairs."+
                        " Use Okey Joker (which is +1 same color as central Gosterme card)"+
                        " or Fake Joker to substitute any needed card in your combination.";
locale.tr.RulesDetails = "Diğer alternatifler; (2X3’lü+2X4’lü), (2X5‘li sıralı+1X4‘lü)."+
                        " (“Okey” göstermenin +1 üstüdür. Joker gibi kullanılır."+
                        " “Sahte okey” ise okey taşının yerinde kullanılır)."+
                        " 3 ve 4 lüler sıralı ve veya asker olabilir.";

// Protocol

locale.tr.player_left         = "Oyundan kaçış";
locale.tr.okey_revealed       = "Açtığı el";
locale.tr.okey_round_ended    = "Tamamladığı oyun";
locale.tr.okey_turn_timeout   = "Zamanında oynamama";
locale.tr.wrong_reveal        = "Yanlış açma";
locale.tr.okey_game_started   = "Oynadığı oyun";

currentLocale = "en";

function i18n(word) {
    var translation = locale[currentLocale][word];
    return (translation == null) ? word : translation;
}
