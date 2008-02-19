abstract EnglishAbs = 
  Core,
  ExtraEngAbs [Tense, Ant, Cl, RCl, QCl, S, RS, QS,
               UncNegCl, UncNegQCl, UncNegRCl],

--  Lexicon

  Lexicon    [N3, distance_N3, 
	      VQ, wonder_VQ, 
	      V2A, paint_V2A, 
	      VA, become_VA],
  BigLexEngAbs,
  FraCaSLexicon

 ** {

fun who_RP : RP ;

fun neither_Det : Det ;

fun several_Det : Det ;

fun at8least_Predet : Predet ;

}