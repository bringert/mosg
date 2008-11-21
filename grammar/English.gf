--# -path=.:alltenses:time
concrete English of Abstract = 
  ParseEng -
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Sentence
    EmbedQS,
-- Structural
    part_Prep, possess_Prep, all_Predet, not_Predet,
-- Verb
    UseVS,
-- Extra
    VPI, ListVPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV
   ],

  FraCaSLexiconEng

** open ParadigmsEng, MorphoEng in {

lin TNoPunct phr = { s = phr.s } ;

lin all_Det = mkDeterminer plural "all" ;

}