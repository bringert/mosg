concrete Norwegian of Abstract = 

  GrammarNor - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep,
-- Verb
    UseVS,
]

  , LexiconNor

** open ResNor, MorphoNor in {

lin

    nobody_NP = regNP "ingen" "ingens" SgUtr ;

    no_Det = {s = \\_ => genderForms "ingen" "inget" ; n = Sg ; det = DIndef} ;

}