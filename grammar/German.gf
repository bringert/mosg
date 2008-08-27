concrete German of Abstract = 

  GrammarGer - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep,
-- Verb
    UseVS
]

  , LexiconGer

** open MorphoGer in {

lin

    nobody_NP = nameNounPhrase {s = caselist "niemand" "niemanden" "niemandem" "niemands"} ;

    no_Det = {
      s = \\g,c => "kein" + pronEnding ! GSg g ! c ;  
      n = Sg ;
      a = Strong
      } ;

}