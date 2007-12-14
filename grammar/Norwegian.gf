--# -path=.:present:prelude
concrete Norwegian of NorwegianAbs = 

  GrammarNor - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP
-- Text

],

  LexiconNor

** open ResNor, MorphoNor in {

lin

    nobody_NP = regNP "ingen" "ingens" SgUtr ;

    no_Det = {s = \\_ => genderForms "ingen" "inget" ; n = Sg ; det = DIndef} ;

}