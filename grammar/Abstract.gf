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

--  , Lexicon

  , Lexicon [N3, distance_N3, 
	      VQ, wonder_VQ, 
	      V2A, paint_V2A, 
	      V2Q, ask_V2Q,
	      V2V, beg_V2V,
	      V2S, answer_V2S,
	      VA, become_VA]
  , BigLexEngAbs
  , FraCaSLexicon

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

fun only_AdV : AdV ;

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