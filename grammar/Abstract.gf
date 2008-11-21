abstract Abstract = 

  ParseEngAbs -
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

  FraCaSLexicon

** {

flags startcat = Text;

--
-- Language-independent additions
--

fun TNoPunct : Phr -> Text ;

-- It's much easier to interpret all_Det than all_Predet
fun all_Det : Det ;

}