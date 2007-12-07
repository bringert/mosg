--# -path=.:present:prelude
concrete English of EnglishAbs = 

  GrammarEng - 
   [
{-
-- Noun
    SentCN, PossPron,
-- Verb
    VQ, ComplVQ, UseVQ,
    VS, ComplVS, UseVS,
    VV, ComplVV,
    V2A, ComplV2A,
    VA, ComplVA,
-- Adjective
    SentAP, AdAP,
-- Adverb
    AdvSC,
-- Sentence
    SC, EmbedQS, EmbedS, EmbedVP, PredSCVP, 
    SlashPrep, SlashVVV2, SlashVS,
-- Question
    QuestIAdv, QuestIComp,
-- Structural
    there_Adv, there7to_Adv,
    that_NP, these_NP, this_NP, those_NP,
    few_Det, many_Det, much_Det,
    can8know_VV, can_VV, must_VV, want_VV,
    that_Quant, this_Quant,
-- Idion
    ImpersCl, GenericCl,
    CleftAdv, ProgrVP, ImpPl1,
-}
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP
-- Text

],

  ExtraEng [Tense, Ant, Cl, RCl, QCl, S, RS, QS,
               UncNegCl, UncNegQCl, UncNegRCl],

--  LexiconEng

  LexiconEng [N3, distance_N3, 
	      VQ, wonder_VQ, 
	      V2A, paint_V2A, 
	      VA, become_VA],
  BigLexEng,
  FraCaSLexiconEng

** open ResEng, MorphoEng in {

lin
    who_RP = {
      s = table {
        RC Gen => "whose" ; 
        RC _   => "who" ;
        RPrep  => "whom"
        } ;
      a = RNoAg
      } ;

    nobody_NP = regNP "nobody" Sg ;

    no_Det = mkDeterminer Sg "no" ;

}