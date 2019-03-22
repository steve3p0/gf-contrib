resource MiniResSrp = open Prelude in {

  flags coding=utf8 ;

  param
    Gender = Masc | Fem ;
    Number = Sg | Pl ;
    Case   = Nom | Acc ;
    Person = Per1 | Per2 | Per3 ;

    Agreement = Agr Gender Number Person ;
    ClitAgr   = CAgrNo | CAgr Agreement ;

    VForm = VInf | VPres Number Person ;

  oper
    genNumStr : Type = Gender => Number => Str ;
    
    ------------------
    -- Noun ----------
    NP = {
      s : Case => {clit,obj : Str ; isClit : Bool} ;
      a : Agreement
      } ;

    Noun : Type = {s : Number => Str ; g : Gender} ;

    mkNoun : Str -> Str -> Gender -> Noun = \sg, pl, g -> {
      s = table {
        Sg => sg ; 
        Pl => pl
        } ;
      g = g
      } ;

    --        (Singular Nouns)              ()
    regNoun : Str -> Gender -> Noun = \sg,  g -> mkNoun sg (sg + "и") g;

    -- Plural Nouns (Nominative Case)
    smartGenNoun : Str -> Gender -> Noun = \stem, g -> case stem of {
      stem1 + v@("а")      => mkNoun  stem  (stem1  +     "е")    g ; -- јабука -> јабуке
      stem2 + v@("о"|"е")  => mkNoun  stem  (stem2  +     "а")    g ; -- пиво   -> пива,   море     -> мора
      stem3 + v@("к"|"ац") => mkNoun  stem  (stem3  +     "ци")   g ; -- дечак  -> дечаци, мушкарац -> мушкарци
      stem4 + v@("да")     => mkNoun  stem  (stem4  +     "дице") g ; -- звезда -> звездаце
      stem5 + v@("л")      => mkNoun  stem  (stem5  + v + "е")    g ; -- бицикл -> бицикле
      stem6 + v@("д")      => mkNoun  stem  (stem6  + v + "ова")  g ; -- брод   -> бродова
      stem7 + v@("з")      => mkNoun  stem  (stem7  + v + "ови")  g ; -- воз    -> возови
      _                    => regNoun stem g -- everything else 
      } ;


    -- TODO: What is this?
    smartNoun : Str -> Noun = \vinho -> case vinho of {
      cas   + "Xa"  => regNoun vinho Fem ;
      vinh  + "Xo"  => regNoun vinho Masc ;
      falc  + "Xão" => mkNoun vinho (falc + "Xões") Masc ; -- other rules depend on stress, can this be built with gf?
      artes + "Xã"  => regNoun vinho Fem ;
      líque + "Xn"  => regNoun vinho Masc ;
      obu   + "X2s"  => mkNoun vinho (vinho + "Xes") Masc ;
      can   + "Xil" =>
        mkNoun vinho (can + "Xis") Masc ; -- what about fóssil?
      _           => smartGenNoun vinho Masc
      } ;


    -- smartNoun : Str -> Noun = \vinho -> case vinho of {
    --   cas   + "a"  => regNoun vinho Fem ;
    --   vinh  + "o"  => regNoun vinho Masc ;
    --   falc  + "ão" => mkNoun vinho (falc + "ões") Masc ; -- other rules depend on stress, can this be built with gf?
    --   artes + "ã"  => regNoun vinho Fem ;
    --   líque + "n"  => regNoun vinho Masc ;
    --   obu   + "2s"  => mkNoun vinho (vinho + "es") Masc ;
    --   can   + "il" =>
    --     mkNoun vinho (can + "is") Masc ; -- what about fóssil?
    --   _           => smartGenNoun vinho Masc
    --   } ;

    mkN = overload {
      mkN : Str -> Noun                     = smartNoun ;
      mkN : Str -> Gender -> Noun           = smartGenNoun ;
      mkN : Str -> Str    -> Gender -> Noun = mkNoun ;
      } ;
    --- Nouns ----


    ProperName : Type = {s : Str ; g : Gender} ;

    mkPN : Str -> Gender -> ProperName = \s,g -> {s = s ; g = g} ;
    -- Pron
    Pron : Type = {s : Case => Str ; a : Agreement} ;

    iMasc_Pron : Pron = {
      s = table {Nom => "ја" ; Acc => "ме"} ;
      a = Agr Masc Sg Per1
      } ;

    youMascSg_Pron : Pron = {
      s = table {Nom => "ти" ; Acc => "те"} ;
      a = Agr Masc Sg Per2
      } ;

    weMasc_Pron : Pron = {
      s = table {Nom => "ми" ; Acc => "нас"} ;
      a = Agr Masc Pl Per1
      };

    youMascPl_Pron : Pron = {
      s = table {Nom => "ви" ; Acc => "вас"} ;
      a = Agr Masc Pl Per2
      } ;

    genderPron : Gender -> Pron -> Pron ;
    genderPron g pr = case pr.a of {
      (Agr _ n pe) => {s = pr.s ; a = Agr g n pe}
      } ;

    employNP : Case -> NP -> Str = \c,np ->
      let nps = np.s ! c in case nps.isClit of {
        True => nps.clit ;
        _    => nps.obj
      } ;

    ---
    -- Adjective
    Adjective : Type = {s : genNumStr ; isPre : Bool} ;

    mkAdjective : (_,_,_,_ : Str) -> Bool -> Adjective = \bom,boa,bons,boas,p -> {
      s = table {
        Masc => table {Sg => bom ; Pl => bons} ;
        Fem  => table {Sg => boa ; Pl => boas}
        } ;
      isPre = p
      } ;

    regAdjective : Str -> Adjective = \preto -> case preto of {
      pret + "o" =>
        mkAdjective preto (pret + "a") (preto + "s") (pret + "as") False ;
      pret + "e" =>
        mkAdjective preto preto (preto + "s") (preto + "s") False ;
      _          => mkAdjective preto preto preto preto False
      } ;

    preAdjective : Str -> Bool -> Adjective = \preto,b ->
      let pretoA = regAdjective preto in
      case b of {
        True => preA pretoA ;
        _    => pretoA
      } ;

    mkA = overload {
      mkA : Str             -> Adjective         = regAdjective ;
      mkA : Str             -> Bool -> Adjective = preAdjective ;
      mkA : (_,_,_,_ : Str) -> Bool -> Adjective = mkAdjective ;
      } ;

    preA : Adjective -> Adjective
      = \a -> {s = a.s ; isPre = True} ;

    ---
    -- Verb
    VP = {
      verb : Verb ;
      clit : Str ;
      clitAgr : ClitAgr ;
      compl : Agreement => Str ;
      } ;

    Verb : Type = {s : VForm => Str } ;

    agrV : Verb -> Agreement -> Str = \v,a -> case a of {
      Agr _ n p => v.s ! VPres n p
      } ;

    neg : Bool -> Str = \b -> case b of {True => [] ; False => "не"} ;

    biti_V = mkV "бити" "сам" "си" "је" "смо" "сте" "су" ;
    --estar_V = mkV "estar" "estou" "está" "estamos" "estão" ;

    mkVerb : (_,_,_,_,_,_,_ : Str) -> Verb =
      --\amar,amo,ama,ama,amam,amamos,amam -> {
      \voleti,volim,volis,voli,volite,volimo,vole -> {
      s = table {
           VInf     => voleti ;
           VPres Sg Per1 => volim ;
           VPres Sg Per2 => volis ;
           VPres Sg Per3 => voli ;
           VPres Pl Per1 => volite ;
           VPres Pl Per2 => volimo ;
           VPres Pl Per3 => vole
        -- VInf     => amar ;
        -- VPres Sg Per1 => amo ;
        -- VPres Sg Per2 => ama ;
        -- VPres Sg Per3 => ama ;
        -- VPres Pl Per1 => amam ;
        -- VPres Pl Per2 => amamos ;
        -- VPres Pl Per3 => amam
        } ;
      } ;

    smartVerb : Str -> Verb = \inf -> case inf of {
    -- stem   inf+suffix                   1st P Sing  2nd P Sing
      part + v@("ати")       => mkVerb inf (part+"ам") (part+"аш") (part+"а") (part+"амо") (part+v+"ате") (part+"ају") ;
      part + v@("ети"|"ити") => mkVerb inf (part+"им") (part+"иш") (part+"и") (part+"имо") (part+v+"ите") (part+"е") ;
      _ => mkVerb inf inf inf inf inf inf inf
      } ;

    mkV = overload {
      mkV : Str -> Verb = smartVerb ;
      mkV : (_,_,_,_,_,_,_ : Str) -> Verb = mkVerb ;
      } ;

    Verb2 : Type = Verb ** {c : Case ; p : Str} ;

    mkV2 = overload {
      mkV2 : Str -> Verb2 =
        \s   -> mkV s ** {c = Nom ; p = []} ;
      mkV2 : Str -> Case -> Verb2 =
        \s,c -> mkV s ** {c = c ; p = []} ;
      mkV2 : Str -> Str -> Verb2 =
        \s,p -> mkV s ** {c = Nom ; p = p} ;
      mkV2 : Str  -> Case -> Str -> Verb2 =
        \s,c,p -> mkV s ** {c = c ; p = p} ;
      mkV2 : Verb -> Verb2 =
        \v -> v ** {c = Nom ; p = []} ;
      mkV2 : Verb -> Case -> Verb2 =
        \v,c -> v ** {c = c ; p = []} ;
      mkV2 : Verb -> Str -> Verb2 =
        \v,p -> v ** {c = Nom ; p = p} ;
      mkV2 : Verb -> Case -> Str -> Verb2 =
        \v,c,p -> v ** {c = c ; p = p} ;
      } ;

    ---
    -- Adverb
    Adverb : Type = {s : Str} ;

    mkAdv : Str -> Adverb = \s -> {s = s} ;

    ---
    -- Det
    -- [ ] is this ok por port?
    adjDet : Adjective -> Number -> {s : Gender => Case => Str ; n : Number} =
      \adj,n -> {
        s = \\g,c => adj.s ! g ! n ;
        n = n
      } ;
    ---
    -- Prep
    Prep : Type = {s : genNumStr } ;
    no_Prep : Prep = { s = table {
                         Masc => table {
                           Sg => "no" ;
                           Pl => "nos"
                           } ;
                         Fem => table {
                           Sg => "na" ;
                           Pl => "nas"
                           }
                         } ;
      } ;

}