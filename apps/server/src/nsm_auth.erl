-module(nsm_auth).
-compile(export_all).
-include_lib("db/include/config.hrl").
-include_lib("kvs/include/user.hrl").
-include_lib("db/include/user.hrl").
-include_lib("server/include/log.hrl").

login(Data) ->
    UserName = proplists:get_value(username, Data),
    Password = proplists:get_value(password, Data),
    HashedPassword = utils:sha(Password),

    Reply =
        case kvs:get(user,UserName) of
            {ok, #user{password = HashedPassword, username = U} = User } ->
                case User#user.status of
                    ok ->
%                        nsm_users:init_mq_for_user(UserName),
%                        nsx_msg:notify([user, init], User),
                        {ok, U};
                    not_verified ->
                        {error, not_verified};
                    banned ->
                        {error, banned};
                    _ ->
                        {error, unknown}
                end;
            {ok, _} ->
                {error, incorrect_password};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found}
        end,
    Reply.

get_user_info(UserId) ->
    Reply =
        case kvs:get(user,UserId) of
            {ok, User} ->
                UserInfo = build_user_info(User),
                {ok, UserInfo};
            {error, not_found} ->
                {error, user_not_found};
            {error, notfound} ->
                {error, user_not_found}
        end,
    Reply.

get_all_user() -> {ok, kvs:all(user)}.

generate_token(User) ->
    Token = generate_token0(),
    Res = auth_server:store_token(0, Token, User),
    ?INFO("with result :~p~n", [Res]),
    Token.

generate_token0() -> T0 = crypto:rand_bytes(100), T = base64:encode(T0), T.

build_user_info(#user{username = UserName,
                      id = Name,
                      surnames = Surname,
                      birth = Age,
                      sex = Sex} = User) ->
    #user_info{username = UserName,
               name = Name,
               surname = Surname,
               age = Age,
               avatar_url = get_avatar(User, small),
               sex = Sex}.

get_avatar(#user{avatar = Avatar}, Size) ->
    get_avatar(Avatar, Size);
get_avatar(Avatar, Size) ->
    case Avatar of
        #avatar{big = Big} when Size =:= big -> Big;
        #avatar{small = Small} when Size =:= small -> Small;
        #avatar{tiny = Tiny} when Size =:= tiny -> Tiny;
        _ -> case Size of
                 big -> "/images/no_avatar_big.jpg";
                 small -> "/images/no_avatar_small.jpg";
                 tiny -> "/images/no_avatar_tiny.jpg"
             end
    end.

names() ->
   ["pinar","betul","eda","lale","ilgin","alp","ayberk","vural","ozan","doruk",
    "duman","boran","dursun","taner","uzay","ali","musa","halit","yusuf","isa",
    "asena","aysu","konca","ceren","oylum","filiz","ezgi","ece","sevil","damla",
    "bahar","arzu","dilara","esra","leyla","jale","fatma","irem","yasmin","zeynep",
    "magnolia","jenifer","roksolana","tsering","suomi"].

surnames() ->
   ["ozcelik","acar","ozgur","ozkan","tez","ustel",
    "mehmet","akbulut","arslan","avci","ayhan","basturk","caglar","celik","cetinkaya","demir",
    "dikmen","acar","dogan","ekinci","elmas","erdem","erdogan","guler","gunes","ilhan",
    "inan","karaca","karadag","kaya","kemal","keskin","koc","korkmaz","mestafa","osman",
    "ozbek","ozcan","ozdemir","ozden","ozturk","pasa","polat","sezer","sahin","sen",
    "simsek","tekin","tosun","tunc","turan","unal","yalcin","yazici","yildirim","yilmaz"].

ima_gio(N) -> {MD5,Name,Surname} = lists:nth(N,imagionary_users()), Name ++ "_" ++ Surname.
ima_gio(N,L) -> {MD5,Name,Surname} = lists:nth(N,L), Name ++ "_" ++ Surname.

imagionary_users() ->
    List = [{crypto:md5(X++Y),X,Y}||X<-names(),Y<-surnames()],
    lists:keysort(1,List).

imagionary_users2() ->
    List = [{crypto:md5(X),X}||X<-imagionary_users_new()],
    lists:keysort(1,List).

ima_gio2(N) -> {MD5,Name} = lists:nth(N,imagionary_users2()), Name.
ima_gio2(N,L) -> {MD5,Name} = lists:nth(N,L), Name.

imagionary_users_new() -> ["1sen__olma","46ruhikizi","516tektas","54seven_","8balim","8bizim","adanali_puan_b",
"akdeniz_aksami","ali_acar","ali_acar","ali_akbulut","ali_arslan","ali_avci","ali_ayhan","alin_yazim",
"alp_ozgur","alp_ozkan","alp_ozturk","alp_pasa","alp_polat","alp_sahin","alp_sen","alp_sezer","alp_simsek",
"altin_kalpler_","anisin_canims","antakya_iskend","arzu_polat","arzu_sahin","arzu_sen","arzu_sezer",
"arzu_simsek","asena_dikmen","asena_dogan","asena_ekinci","asena_elmas","asiklar_limani",
"asiklar_limani","asil_ve_soylu","asilbayan","asildostlar","astsubay","ask_i","aska_dair",
"aska_dogan_gun","atesli_bayanla","atesli_wip_soh","atesli","atesli__olgun","atesli_dusler",
"atesli_seksi","avi_melekler_","ayberk_mehmet","ayberk_mestafa","ayberk_osman","ayberk_ozbek",
"aylincik90140","ayrilamayiz","ayrilmaz_dostl","aysu_polat","aysu_sahin","aysu_sen","aysu_sezer",
"aysu_simsek","aysu_tekin","babacan","bahar","bahar_acar","bahar_acar","bahar_akbulut","bahar_arslan",
"bahar_avci","bahar_ayhan","bahar_basturk","baharca_35","bahcesi","bahcem__","bak_yesilyesil",
"bakma_gel","bal_petegim_oy","bereketi15","betul_caglar","betul_celik","betul_cetinkaya",
"betul_demir","betul_dikmen","betul_dogan","betul_ekinci","betul_elmas","bir_sevda_masa",
"birak","birdaha_asla","boran_karaca","boran_karadag","boran_kaya","boran_kemal","boran_keskin",
"burcu11","bursa_bursa_oy","cafesimercan","can_dost","canemekliler","canlarvatan","cennet_gozlu",
"cennetdeki_mel","cenneti_buldum","ceren_kemal","ceren_keskin","ceren_koc","ceren_korkmaz",
"ceren_mehmet","cikmaz","cypress","cagla_yure","cakil_tasi","canakkale_truv","catkapi",
"cikmazi1","d_u_z_c_eli","damla_basturk","damla_caglar","damla_celik","damla_cetinkaya",
"damla_demir","damla_dikmen","delice","deniz_mavisi","dersim","dilara_ozcan","dilara_ozcelik",
"dilara_ozdemir","dinle_sevgili","diyari","dogunun","doruk_caglar","doruk_celik","doruk_cetinkaya",
"doruk_demir","dost_kervani","dostlar_mekani","dostlarin_","dostluk","dostluk_baglari","dort",
"dudaktan_kalbe","duman_ozkan","duman_ozturk","duman_pasa","duman_polat","duman_sahin","dusler",
"dusler_sokagi","e__x_i_hatu","ece_gunes","ece_ilhan","ece_inan","ece_karaca","ece_karadag",
"ecem_net","eda_acar","eda_acar","eda_akbulut","eda_arslan","eda_avci","efsane_oyuncu",
"ek_yurek","elitler5","emekliler","es_es_asiklari","eskisehir_","esmer_aci","esmer_meleg",
"esra_yazici","esra_yildirim","esra_yilmaz","evdali","evlere_senlikk","evliler_izmir45",
"evlilerde_seve","eylul_aksamlar","ezgi_acar","ezgi_acar","ezgi_akbulut","ezgi_arslan",
"ezonun","fark_var","fatma_kemal","fatma_keskin","fatma_koc","fatma_korkmaz","fatma_mehmet",
"fatma_mestafa","feshane_1","filiz_ozkan","filiz_ozturk","filiz_pasa","filiz_polat",
"filiz_sahin","firari_asklar","flas_flas_esen","g_i_r_g_i_r_i_","gemliklinin","gencaga45",
"gercek_dost","gizem_puan_oyu","gizli","gonul_sevd","gozlerdetutsak","gozlumsiyahbey","gonul_bah",
"gonul_kahvesi_","gonul_limani","gozlumsamanyolu","gumbur","gunesim__puan_","gurbet",
"gurur_duru_oke","gul_yuzlum","gulusu_yaralim","h_a_r_i_k_a_oy","h_a_w_a_l_i__p","hayal_kahve",
"hayall_gozlumm903eyl","hayaller_men","hillaskin","hoporoz","huzur_limani","i_n_a_t_c_i___",
"i3ir_umuttur_y","ikimizin_sevda","ikinci","inadina_soh","inci_adasi","isa_demir","isa_dikmen",
"isa_dogan","isa_ekinci","izmit_kocaeli2","jale_polat","jale_sahin","jale_sen","jale_sezer",
"jale_simsek","k_i_r_c_i_c_e_","kacamak","kaderim","kaderimsin_sen1","kalbimde","kalp_ayazi_",
"karabagli","karadeniz_ordu","kastamonulu","kelebek_2_","kirik_kalp_","kirmizi_kral",
"kizil_partizan","kizlari112537meleq_s","klasokeyci_06","kocaeli_turnuv","lacivert_sevda",
"lale_devrii","leyla_ayhan","leyla_basturk","leyla_caglar","leyla_celik","leyla_cetinkaya",
"limon_cicekler","mahmur_gozlum_","mardin_1_a_d_a","marmara_yildiz","marurgoz","masal_perisi_o",
"mavi_dusss","mavi_gul","mekani1","melekler_ad","musa_yildirim","musa_yilmaz","mutevazi",
"nartanesii_e_c","neredesin_gels","neselivadi","nil_nehrim","nilay4500kelebek_pua",
"o_z_a_n_oyun_o","omrum_senin","ortayas_istanb","ox_aft_asill","oylum_acar","oylum_acar",
"oylum_akbulut","oylum_arslan","oylum_avci","oyun1dost","ozan_yazici","ozan_yildirim",
"ozan_yilmaz","olmezbursa","olumsuz_ask","ozelsin_","ozgurlukgunesi","p_a_l_m_i_y_e_",
"p_a_l_m_i_y_e12__","p_u_a_n_41_sud","pilot","pinar_acar","pinar_acar","pinar_akbulut",
"pinar_arslan","puan","puan_nisanyagm","puan_oyun_tatl","r_u_z_g_a_r0guz_","rdu_evicok_ozel",
"s_e_x_x_0kara_se","sabirmelegi","sahte_olmayan_","sebebimsin_oyu","seckin_oyun_pu",
"seheryel","sen_ve_ben_oyu","sevda_sokagi_4","sevdali_yurekl","sevdim_oyun_od",
"sevdim0atesli","sevenler11","sevgi_denizi_y","sevgi_seli_oyu","sevil_guler",
"sevil_gunes","sevil_ilhan","sevil_inan","seviyorumsteve","sevmek_yurek_i",
"sevmek_yurekte","sibel","sivases","son__liman____","sonn__nefesims","sonn_nefess",
"sozun_bittigi","stylesevgi","subay","surgun_sevda","suuuessenli","super45",
"t_u_t_k_ucan","taner_elmas","taner_erdem","taner_erdogan","taner_guler","taner_gunes",
"taner_ilhan","tatildeyiz","tatli_cadi","tebessum_alyan","tesbihin","trakya",
"ugur_bocegim","uludagirtek_s","umutlar","uykulugozler","vesevgi","vural_celik",
"vural_cetinkaya","vural_demir","vural_dikmen","vural_dogan","vural_ekinci",
"yalniz_kalpler","yaman_puan_oda","yarimden","yenidostlar2","yenilmez11","yersu",
"yureginizin_se","yusuf_osman","yusuf_ozbek","yusuf_ozcan","yusuf_ozcelik",
"yusuf_ozdemir","yusuf_ozden","yureqinle_","z_i_r_v_e__","zakkumm_zakkum","zumrutgozlu"].
