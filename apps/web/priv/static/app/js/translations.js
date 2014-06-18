locale = { en: {}, tr: {} };

locale.tr.Score       = "Puan";
locale.tr.Online      = "Oynayanlar";
locale.tr.Games       = "Oyunlar";
locale.tr.Rules       = "Kurallar";
locale.tr.Quota       = "Kota";
locale.tr.Kakush      = "Kakuş";
locale.tr.Login       = "Üye Ol";
locale.tr.Statistics  = "İstatistikleri";
locale.en.EditMessage = "Write some text here";
locale.tr.EditMessage = "Burada bazı metin yazmak";
locale.en.GameChat    = "This is In-Game chat";
locale.tr.GameChat    = "Oyun sohbet";
locale.en.PrivateChat = "is waiting for chat with you";
locale.tr.PrivateChat = "seninle sohbet için bekliyor";

currentLocale = "tr";

function i18n(word) {
    var translation = locale[currentLocale][word];
    return (translation == null) ? word : translation;
}
