--# -path=.:present:prelude
abstract EnglishAbs = 
  Core,
  ExtraEngAbs [Tense, Ant, Cl, RCl, QCl, S, RS, QS,
               UncNegCl, UncNegQCl, UncNegRCl],

  Lexicon

{-
  Lexicon [N3, distance_N3], -- missing from BigLexEng
  BigLexEngAbs
-}

 ** {

fun who_RP : RP ;

}