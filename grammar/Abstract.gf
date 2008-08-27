abstract Abstract = 

--
-- Language-independent stuff from the resource library
--

  Grammar - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep,
-- Verb
    UseVS
   ]
--   , Time

--
-- Lexica
--

  , Lexicon
--  , FraCaSLexicon

--
-- English-specific resource grammar constructs
--

  , ExtraEngAbs - [VPI, ListVPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV]


** {

flags startcat = Text;

--
-- Language-independent additions
--

fun TNoPunct : Phr -> Text ;

fun nobody_NP : NP ;

fun anybody_NP : NP ;

fun anything_NP : NP ;

fun nothing_NP : NP ;

fun no_Det : Det ;

fun most_Det : Det ;

fun both_Det : Det ;

fun a8few_Det : Det ;

fun any_Predet : Predet ;

fun another_Predet : Predet ;

fun exactly_AdN : AdN ;

fun at8most_AdN : AdN ;

fun at8least_AdN : AdN ;

--
-- English-specific additions
--

fun either_Det : Det ;

fun neither_Det : Det ;

fun several_Det : Det ;

fun should_VV : VV ;

fun BareInfVS : VS -> NP -> VP -> VP ;

fun VerbCN : V -> CN -> CN ; -- running man

-- FIXME: problematic with NoNum
--fun NumOfNP : Num -> NP -> NP ; -- ten of the dogs

}