--# -path=.:alltenses:time
concrete Swedish of Abstract = 

  GrammarSwe - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep,
-- Verb
    UseVS
],

  LexiconSwe

{-
  LexiconSwe [N2, N3, brother_N2, father_N2, mother_N2, distance_N3],
  BigLexSwe
-}

** open ResSwe, MorphoSwe in {

lin TNoPunct phr = { s = phr.s } ;

lin

    nobody_NP = regNP "ingen" "ingens" SgUtr ;

    no_Det = {s = \\_ => genderForms "ingen" "inget" ; n = Sg ; det = DIndef} ;

}