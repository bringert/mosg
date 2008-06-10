--# -path=.:alltenses:time
concrete English of EnglishAbs = 

  GrammarEng - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep,
-- Verb
    UseVS,
-- overridden
    everybody_NP, somebody_NP,
    every_Det, only_Predet
],

  TimeEng,

  ExtraEng - [VPI, ListVPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV],

--  LexiconEng

  LexiconEng [N3, distance_N3, 
	      VQ, wonder_VQ, 
	      V2A, paint_V2A, 
	      V2Q, ask_V2Q,
	      V2V, beg_V2V,
	      V2S, answer_V2S,
	      VA, become_VA],
  BigLexEng,
  FraCaSLexiconEng

** open ResEng, MorphoEng, Prelude in {

lin TNoPunct phr = { s = phr.s } ;

lin
    who_RP = {
      s = table {
        RC Gen => "whose" ; 
        RC _   => "who" ;
        RPrep  => "whom"
        } ;
      a = RNoAg
      } ;

    either_Det = mkDeterminer Sg "either" ;

    neither_Det = mkDeterminer Sg "neither" ;

    nobody_NP = variants { regNP "nobody" Sg; regNP "noone" Sg; regNP ["no one"] Sg };

    anybody_NP = variants { regNP "anybody" Sg; regNP "anyone" Sg };

    anything_NP = regNP "anything" Sg;

    nothing_NP = regNP "nothing" Sg;

    no_Det = variants { mkDeterminer Sg "no"; mkDeterminer Pl "no" } ;

    most_Det = mkDeterminer Pl "most";

    both_Det = mkDeterminer Pl "both";

    a8few_Det = mkDeterminer Pl ["a few"];

    any_Predet = ss "any" ;

    another_Predet = ss "another" ;

    everybody_NP = variants { regNP "everybody" Sg; regNP "everyone" Sg } ;
    somebody_NP = variants { regNP "somebody" Sg; regNP "someone" Sg } ;

    every_Det = variants { mkDeterminer Sg "every"; mkDeterminer Sg "each" };
    several_Det = mkDeterminer Pl "several" ;
    only_Predet = variants { ss "only"; ss "just" };
    exactly_AdN = ss "exactly" ;
    at8least_AdN = ss ["at least"] ;
    at8most_AdN = ss ["at most"] ;

    -- FIXME: many strange forms
    should_VV = {
      s = table {
	VVF VInf => "should" ;
	VVF VPres => "should" ;
	VVF VPPart => "should" ;
	VVF VPresPart => "should" ;
	VVF VPast => "should" ;
	VVPastNeg => "shouldn't" ;
	VVPresNeg => "shouldn't"
	} ;
      isAux = True
    } ;

    BareInfVS vs np vp = insertObj (\\_ => np.s!Acc ++ infVP True vp np.a) (predV vs) ;

    VerbCN v cn = {s = \\n,c => v.s ! VPresPart ++ cn.s ! n ! c };

{-
    NumOfNP num np = {
      s = \\c => num.s ++ "of" ++ np.s ! c ; 
      a = agrP3 num.n
      } ;
-}

}