abstract Abstract = 

  ParseEngAbs -
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

  FraCaSLexicon

** {

flags startcat = Text;

--
-- Language-independent additions
--

fun TNoPunct : Phr -> Text ;

}