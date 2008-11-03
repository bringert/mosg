--# -path=.:alltenses:time
concrete English of Abstract = 
  ParseEng -
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep,
-- Verb
    UseVS,
-- Extra
    VPI, ListVPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV
   ],

  FraCaSLexiconEng

** {

lin TNoPunct phr = { s = phr.s } ;

}