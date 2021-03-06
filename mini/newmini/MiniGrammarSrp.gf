concrete MiniGrammarSrp of MiniGrammar = open MiniResSrp, Prelude in {
  lincat
    Utt = {s : Str} ;
    Pol = {s : Str ; b : Bool} ;
    S  = {s : Str} ;
    Cl = {s : Bool => Str} ;
    VP = MiniResSrp.VP ;
    Adv = Adverb ;
    NP = MiniResSrp.NP ;
    Conj = {s : Str} ;
    
    -- Det
    Det = {s : Gender => Case => Str ; n : Number} ;
    -- Det = zeroDet | everyDet ;
    -- zeroDet = {s = "" } ;
    --           --{s = table {Nom => "оне" ; Acc => "њих"} ; ;
    -- -- zero = { s = table {
    -- --     Sg => sg ; 
    -- --     Pl => pl
    -- --     } ;
    -- --   }
    --everyDet = {s : Gender => Case => Str ; n : Number} ;
    
    --Prep = {s : Str} ;
    Prep = MiniResSrp.Prep;
    
    AP = Adjective ;
    CN = Noun ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = ProperName ;
    Pron = MiniResSrp.Pron ;

  lin
    -- Phrase ------------------------------------------
    UttS s = s ;
    UttNP np = ss (employNP Nom np) ;
    -- Sentence
    UsePresCl pol cl = {
      s = pol.s ++ cl.s ! pol.b
      } ;
    PredVP np vp = let subj = (np.s ! Nom).obj ;
                       obj = vp.compl ! np.a ;
                       clit = vp.clit ;
                       verb = agrV vp.verb np.a
      in {
        s = \\b => subj ++ clit ++ neg b ++ verb ++ obj
      } ;
    
    -- Verb ---------------------------------------------
    UseV v = {
      verb = v ;
      clit = [] ;
      clitAgr = CAgrNo ;
      compl = \\_ => []
      } ;
    
    ComplV2 v2 np = let nps = np.s ! v2.c in {
      verb = {s = v2.s} ;
      clit = nps.clit ;
      clitAgr = case <nps.isClit,v2.c> of {
        <True,Acc> => CAgr np.a ;
        _          => CAgrNo
        } ;
      compl = \\_ => v2.p ++ nps.obj
      } ;
    
    AdvVP vp adv = vp ** {compl = \\agr => vp.compl ! agr ++ adv.s } ;
    UseAP ap = {
      verb = biti_V;
      clit = [] ;
      clitAgr = CAgrNo ;
      compl = \\agr => case agr of {
        Agr g n _ => ap.s ! g ! n
        }
      } ;

    -- Noun, CN, NP ------------------------------------------------
    UseN n = n ;
    PositA a = a ;
    
    --PrepNP
    PrepNP prep np = case np.a of {
      Agr g n _ => {s = prep.s ! g ! n ++ employNP Nom np}
      } ;

    AdjCN ap cn = case ap.isPre of {
        True => cn ** {s = table {n => ap.s ! cn.g ! n ++ cn.s ! n}} ;
        False => cn ** {s = table {n => cn.s ! n ++ ap.s ! cn.g ! n}}
      } ;

    MassNP cn = {
      s = \\_ => {clit = [] ; obj = cn.s ! Sg ; isClit = False} ;
      a = Agr cn.g Sg Per3
      } ;
    
    --DetCN
    --Det = {s : Gender => Case => Str ; n : Number} ;
    DetCN det cn = {
      s = \\c => {clit = [] ;
                  obj = det.s ! cn.g ! c ++ cn.s ! det.n ;
                  isClit = False
        } ;
      a = Agr cn.g det.n Per3 ;
      } ;

    UsePN pn = {
      s = \\_ => {clit = [] ; obj = pn.s ; isClit = False} ;
      a = Agr pn.g Sg Per3
      } ;

    -- Pron
    UsePron p = {
      s = table {
        Nom => {clit = [] ;
                obj = p.s ! Nom ;
                isClit = False} ;
        Acc => {clit = p.s ! Acc ;
                obj = [] ;
                isClit = True}
        } ;
      a = p.a
      } ;

    i_Pron = iMasc_Pron | genderPron Fem iMasc_Pron ;
    youSg_Pron = youMascSg_Pron | genderPron Fem youMascSg_Pron ;
    
    he_Pron = {
      s = table {Nom => "он" ; Acc => "га"} ;
      a = Agr Masc Sg Per3
      } ;
    she_Pron = {
      s = table {Nom => "oна" ; Acc => "je"} ;
      a = Agr Fem Sg Per3
      } ;
    we_Pron    = weMasc_Pron | genderPron Fem weMasc_Pron ;
    youPl_Pron = youMascPl_Pron | genderPron Fem youMascPl_Pron ;
    they_Pron = {
      s = table {Nom => "они" ; Acc => "њих"} ;
      a = Agr Masc Pl Per2
      }
      | {
        s = table {Nom => "оне" ; Acc => "њих"} ;
        a = Agr Fem Pl Per2
          } ;
    -- Det
    -- TO DO: NO DET

    -- Det 
    -- zero out "a" and "the" determiners
    -- TO DO: Need masc / fem
    -- a_Det     = adjDet ("") ;
    -- aPl_Det   = adjDet (mkAdjective [] [] "" "" True) Pl ;
    -- the_Det   = adjDet (mkAdjective "" "" [] [] True) Sg ;
    -- thePl_Det = adjDet (mkAdjective [] [] "" "" True) Pl ;

    --Det = {s : Gender => Case => Str ; n : Number} ;
    -- DetCN det cn = {
    --   s = \\c => {clit = [] ;
    --               obj = det.s ! cn.g ! c ++ cn.s ! det.n ;
    --               isClit = False
    --     } ;
    --   a = Agr cn.g det.n Per3 ;
    --   } ;

    a_Det     = adjDet (mkAdjective "" "" [] [] True) Sg ;
    aPl_Det   = adjDet (mkAdjective [] [] "" "" True) Pl ;
    the_Det   = adjDet (mkAdjective "" "" [] [] True) Sg ;
    thePl_Det = adjDet (mkAdjective [] [] "" "" True) Pl ;

    every_Det = adjDet (mkAdjective "сваки" "свака" [] [] True) Sg ;


    -- Prep
    in_Prep = no_Prep ;
    on_Prep = no_Prep ;
    with_Prep = {s = \\_ => \\_ => "com"} ;

    -- -- Prep
    -- in_Prep = {s = "у"} ;
    -- on_Prep = {s = "на"} ;
    -- with_Prep = {s = "са"} ;

    -- Conjunction/Disjunction
    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    and_Conj = {s = "и"} ;
    or_Conj = {s = "или"} ;
    -- polarity
    PPos  = {s = [] ; b = True} ;
    PNeg  = {s = [] ; b = False} ;

}
