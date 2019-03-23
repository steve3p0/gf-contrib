concrete MiniLexiconSrp of MiniLexicon = MiniGrammarSrp ** open MiniResSrp in 
{    
	-- Proper Nouns
	lin john_PN = mkPN "Јован" Masc;
	lin paris_PN = mkPN "Парис" Masc;

	-- Adverbs
	lin already_Adv = mkAdv "already" ;
	lin now_Adv = mkAdv "сада" ;

	-- Verbs (V)
	lin come_V = mkV "доћи" ; --!!!
	lin go_V = mkV "ићи" ; --!!!
	lin jump_V = mkV "скочити" ;
	lin live_V = mkV "живети" ;
	lin play_V = mkV "играти" ;
	lin run_V = mkV "трчати" ;
	lin sleep_V = mkV "спавати" ;
	lin swim_V = mkV "пливати" ;
	lin travel_V = mkV "путовати" ;
	lin walk_V = mkV "ходати" ;

	-- Verbs (V2)
	--                        inf      P1S     P2S    P3S     P1P      P2P      P3P
  --lin read_V2 = mkV2 (mkV "читати" "читам" "читаш" "чита" "читамо" "читате" "читају");
  --lin read_V2 = mkV2 (mkV "читати" "читам" "читаш"        "читамо" 	
    lin read_V2       = mkV2 "читати" ;
	lin break_V2      = mkV2 "сломити" ;
	lin buy_V2        = mkV2 "купити" ;
	lin drink_V2      = mkV2 "пити" ;
	lin eat_V2        = mkV2 "јести" ;
	lin find_V2       = mkV2 "наћи" ;
	lin kill_V2       = mkV2 "убити" ;
	lin love_V2       = mkV2 "волети" ;
	lin see_V2        = mkV2 "видети" ;
	lin teach_V2      = mkV2 "учити" ;
	lin understand_V2 = mkV2 "разумети" ;
	lin wait_V2       = mkV2 "чекати" "за" ;

	-- Adjectives
	lin bad_A    = mkA "лош" ;
	lin big_A    = mkA "велик" ;
	lin black_A  = mkA "црн" ;
	lin blue_A   = mkA "Плави" ;
	lin clean_A  = mkA "чист" ;
	lin clever_A = mkA "паметан" ;
	lin cold_A   = mkA "хлад" ;
	lin dirty_A  = mkA "прљав" ;
	lin good_A   = mkA "добар" ;
	lin green_A  = mkA "зелен" ;
	lin heavy_A  = mkA "тешак" ;
	lin hot_A    = mkA "врућ" ;
	lin new_A    = mkA "нов" ;
	lin old_A    = mkA "стар" ;
	lin ready_A  = mkA "спреман" ;
	lin red_A    = mkA "црвен" ;
	lin small_A  = mkA "мали" ;
	lin warm_A   = mkA "топли" ;
	lin white_A  = mkA "бео" ;
	lin yellow_A = mkA "жут" ;
	lin young_A  = mkA "млад" ;

	-- Nouns
	lin animal_N   = mkN "животиња" ;
	lin apple_N    = mkN "јабука" ;
	lin baby_N     = mkN "беба" ;
	lin beer_N     = mkN "пиво" ;
	lin bike_N     = mkN "бицикл" ;
	lin bird_N     = mkN "птица" ;
	lin blood_N    = mkN "крв" ;
	lin boat_N     = mkN "брод" ;
	lin book_N     = mkN "књига" ;
	lin boy_N      = mkN "дечак" ;
	lin bread_N    = mkN "хлеб" "хлеб" Masc;
	lin car_N      = mkN "ауто" "аутомобили" Masc; -- Neuter
	lin cat_N      = mkN "мачка" ;
	lin child_N    = mkN "дете" "деца" Masc; -- NEUTER
	lin city_N     = mkN "град" ;
	lin cloud_N    = mkN "облак" ;
	lin computer_N = mkN "рачунар" "рачунара" Masc ;
	lin cow_N = mkN "крава" ;
	lin dog_N = mkN "пас" "пси" Masc;
	lin fire_N = mkN "пожар" ;
	lin fish_N = mkN "риба" ;
	lin flower_N = mkN "цвет" "цвеће" Fem ; --?
	lin friend_N = mkN "пријатељу" ;
	lin girl_N = mkN "девојка" ;
	lin grammar_N = mkN "граматика" Fem ;
	lin horse_N = mkN "коњ" ;
	lin house_N = mkN "кућа" ;
	lin language_N = mkN "језик" ;
	lin man_N = mkN "мушкарац";
	lin milk_N = mkN "млеко" ;
	lin music_N = mkN "музика" ;
	lin river_N = mkN "река" ;
	lin sea_N = mkN "море" ;
	lin ship_N = mkN "брод" ;
	lin star_N = mkN "звезда" ;
	lin train_N = mkN "воз" ;
	lin tree_N = mkN "дрво" "дрвеће" Masc; -- Neuter
	lin water_N = mkN "вода" ;
	lin wine_N = mkN "вино" ;
	lin woman_N = mkN "жена";
}
