concrete Swedish of SwedishAbs = 

  GrammarSwe - 
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

  LexiconSwe

{-
  LexiconSwe [N2, N3, brother_N2, father_N2, mother_N2, distance_N3],
  BigLexSwe
-}

** open ResSwe, MorphoSwe in {

lin

    nobody_NP = regNP "ingen" "ingens" SgUtr ;

    no_Det = {s = \\_ => genderForms "ingen" "inget" ; n = Sg ; det = DIndef} ;

}