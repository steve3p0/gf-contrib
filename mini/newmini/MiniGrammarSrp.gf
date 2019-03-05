concrete MiniGrammarSrp of MiniGrammar = open MiniResSrp, Prelude in {


  lincat
    Utt = {s : Str} ;
    Adv = Adverb ;
    Pol = {s : Str ; b : Bool} ;
    
    S  = {s : Str} ;
    Cl = {s : Bool => Str} ;
    VP = {verb : GVerb ; compl : Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = {s : Case => Str ; a : Agreement} ;
    Pron = {s : Case => Str ; a : Agreement} ;
    Det = {s : Str ; n : Number} ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = ProperName ;

  lin
    UttS s = s ;
    UttNP np = {s = np.s ! Acc} ;

    UsePresCl pol cl = {
      s = pol.s ++ cl.s ! pol.b
      } ;
    PredVP np vp = {
      s = \\b =>
           np.s ! Nom 
	++ case <b, np.a, vp.verb.isAux> of {
	    <True, Agr Sg Per1,_> => vp.verb.s ! PresSg1 ;
	    <True, Agr Sg Per3,_> => vp.verb.s ! VF PresSg3 ;
	    <True, _          ,_> => vp.verb.s ! PresPl ;
	    <False, Agr Sg Per1,True>  => vp.verb.s ! PresSg1 ++ "не" ;
	    <False, Agr Sg Per3,True>  => vp.verb.s ! VF PresSg3 ++ "не" ;
	    <False, _          ,True>  => vp.verb.s ! PresPl ++ "не" ;
	    <False, Agr Sg Per3,False> => "не" ++ vp.verb.s ! VF Inf ;
	    <False, _          ,False> => "не" ++ vp.verb.s ! VF Inf
	    }
        ++ vp.compl ;
      } ;
      
    UseV v = {
      verb = verb2gverb v ;
      compl = []
      } ;
    ComplV2 v2 np = {
      verb = verb2gverb v2 ;
      compl = v2.c ++ np.s ! Acc
      } ;
    UseAP ap = {
      verb = be_GVerb ;
      compl = ap.s
      } ;
    AdvVP vp adv =
      vp ** {compl = vp.compl ++ adv.s} ;
      
    DetCN det cn = {
      s = table {c => det.s ++ cn.s ! det.n} ;
      a = Agr det.n Per3
      } ;
    UsePN pn = {
      s = \\_ => pn.s ;
      a = Agr Sg Per3
      } ;
    UsePron p =
      p ;
    MassNP cn = {
      s = \\_ => cn.s ! Sg ;
      a = Agr Sg Per3
      } ;
    a_Det = {s = "" ; n = Sg} ;
    aPl_Det = {s = "" ; n = Pl} ;
    the_Det = {s = "" ; n = Sg} ;
    thePl_Det = {s = "" ; n = Pl} ;
    UseN n =
      n ;
    AdjCN ap cn = {
      s = table {n => ap.s ++ cn.s ! n}
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Acc} ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = [] ; b = True} ;
    PNeg  = {s = [] ; b = False} ;

    and_Conj = {s = "и"} ;
    or_Conj = {s = "или"} ;

    every_Det = {s = "свака" ; n = Sg} ;

    in_Prep = {s = "у"} ;
    on_Prep = {s = "на"} ;
    with_Prep = {s = "са"} ;

    i_Pron = {
      s = table {Nom => "ја" ; Acc => "ме"} ;
      a = Agr Sg Per1
      } ;
    youSg_Pron = {
	  s = table {Nom => "ти" ; Acc => "те"} ;
      s = \\_ => "you" ;
      a = Agr Sg Per2
      } ;
    he_Pron = {
      s = table {Nom => "он" ; Acc => "га"} ;
      a = Agr Sg Per3
      } ;
    she_Pron = {
      s = table {Nom => "she" ; Acc => "je"} ;
      a = Agr Sg Per3
      } ;
    we_Pron = {
      s = table {Nom => "ми" ; Acc => "нас"} ;
      a = Agr Pl Per1
      } ;
    youPl_Pron = {
	  s = table {Nom => "ви" ; Acc => "вама"} ;
      a = Agr Pl Per2
      } ;
    they_Pron = {
      s = table {Nom => "они" ; Acc => "them"} ;
      a = Agr Pl Per2
      } ;

}
