--# -path=.:alltenses:time
concrete English of EnglishAbs = 

  GrammarEng - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- overridden
    everybody_NP, somebody_NP,
    every_Det
],

  TimeEng,

  ExtraEng [Tense, Ant, Cl, RCl, QCl, S, RS, QS,
               UncNegCl, UncNegQCl, UncNegRCl],

--  LexiconEng

  LexiconEng [N3, distance_N3, 
	      VQ, wonder_VQ, 
	      V2A, paint_V2A, 
	      VA, become_VA],
  BigLexEng,
  FraCaSLexiconEng

** open ResEng, MorphoEng, Prelude in {

lin
    who_RP = {
      s = table {
        RC Gen => "whose" ; 
        RC _   => "who" ;
        RPrep  => "whom"
        } ;
      a = RNoAg
      } ;

    neither_Det = mkDeterminer Sg "neither" ;

    nobody_NP = variants { regNP "nobody" Sg; regNP "noone" Sg; regNP ["no one"] Sg };

    anybody_NP = variants { regNP "anybody" Sg; regNP "anyone" Sg };

    no_Det = variants { mkDeterminer Sg "no"; mkDeterminer Pl "no" } ;

    both_Det = mkDeterminer Pl "both";

    any_Predet = ss "any" ;

    everybody_NP = variants { regNP "everybody" Sg; regNP "everyone" Sg } ;
    somebody_NP = variants { regNP "somebody" Sg; regNP "someone" Sg } ;

    every_Det = variants { mkDeterminer Sg "every"; mkDeterminer Sg "each" };
    several_Det = mkDeterminer Pl "several" ;
    exactly_AdN = ss "exactly" ;
    at8least_AdN = ss ["at least"] ;
    at8most_AdN = ss ["at least"] ;

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

}