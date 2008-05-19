abstract EnglishAbs = 
  Core,
  ExtraEngAbs - [VPI, ListVPI, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV],

--  Lexicon

  Lexicon    [N3, distance_N3, 
	      VQ, wonder_VQ, 
	      V2A, paint_V2A, 
	      V2Q, ask_V2Q,
	      V2V, beg_V2V,
	      V2S, answer_V2S,
	      VA, become_VA],
  BigLexEngAbs,
  FraCaSLexicon

 ** {

fun who_RP : RP ;

fun either_Det : Det ;

fun neither_Det : Det ;

fun several_Det : Det ;

fun should_VV : VV ;

fun BareInfVS : VS -> NP -> VP -> VP ;

fun VerbCN : V -> CN -> CN ; -- running man

-- FIXME: problematic with NoNum
--fun NumOfNP : Num -> NP -> NP ; -- ten of the dogs

}