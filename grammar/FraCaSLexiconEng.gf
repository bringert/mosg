concrete FraCaSLexiconEng of FraCaSLexicon = CatEng ** open Prelude, StructuralEng, IrregEng, ParadigmsEng in {
lin
  broke_A = mkA "broke" ;
  fourlegged_A = mkA "four-legged" ;
  furious_A = mkA "furious" ;
  indispensable_A = mkA "indispensable" ;
  national_A = mkA "national" ;
  successful_A = mkA "successful" ;
  unemployed_A = mkA "unemployed" ;
  interesting_A = mkA "interesting" ;
  noted_A = mkA "noted" ;

  asleep_A = mkA "asleep" ;

  italian_A = mkA "italian" ;
  portuguese_A = mkA "portuguese" ;

  accountant_N = mkN "accountant" ;
  auditor_N = mkN "auditor" ;
  businessman_N = mkN "businessman" "businessmen" ;
  commissioner_N = mkN "commissioner" ;
  continent_N = mkN "continent" ;
  customer_N = mkN "customer" ;
  delegate_N = mkN "delegate" ;
  demonstration_N = mkN "demonstration" ;
  executive_N = mkN "executive" ;
  graduate_N = mkN "graduate" ;
  inhabitant_N = mkN "inhabitant" ;
  invoice_N = mkN "invoice" ;
  laptop_N = mkN "laptop" ;
  lecturer_N = mkN "lecturer" ;
  lobby_N = mkN "lobby" ;
  meeting_N = mkN "meeting" ;
  memoir_N = mkN "memoir" ;  -- FIXME: plural only
  mortgage_N = mkN "mortgage" ;
  mp_N = mkN "mp" ;
  payrise_N = mkN "payrise" ;
  performance_N = mkN "performance" ;
  proposal_N = mkN "proposal" ;
  species_N = mkN "species" ;
  stockmarket_N = mkN "stock-market" ;
  trader_N = mkN "trader" ;
  workstation_N = mkN "workstation" ;

  irishman_N = mkN "irishman" "irishmen" ;
  italian_N = mkN "italian" ;
  scandinavian_N = mkN "scandinavian" ;

  alan_PN = mkPN "alan" ;
  anderson_PN = mkPN "anderson" ;
  carl_PN = mkPN "carl" ;
  dumbo_PN = mkPN "dumbo" ;
  frank_PN = mkPN "frank" ;
  helen_PN = mkPN "helen" ;
  jones_PN = mkPN "jones" ;
  mickey_PN = mkPN "mickey" ;
  nobel_PN = mkPN "nobel" ;
  pavarotti_PN = mkPN "pavarotti" ;
  peter_PN = mkPN "peter" ;
  smith_PN = mkPN "smith" ;

  socrates_PN = mkPN "socrates" ;

  berlin_PN = mkPN "berlin" ;
  birmingham_PN = mkPN "birmingham" ;
  florence_PN = mkPN "florence" ;
  katmandu_PN = mkPN "katmandu" ;
  portugal_PN = mkPN "portugal" ;
  paris_PN = mkPN "paris" ;
  scandinavia_PN = mkPN "scandinavia" ;

  apcom_PN = mkPN "apcom" ;
  itel_PN = mkPN "itel";
  gfi_PN = mkPN "gfi" ;
  mfi_PN = mkPN "mfi" ;
  mtalk_PN = mkPN "mtalk" ;
  icm_PN = mkPN "icm" ;
  r95103_PN = mkPN "r-95-103" ;
  cia_PN = mkPN "cia" ;
  bt_PN = mkPN "bt" ;

  pc6082_N = mkN "pc-6082" ;
  itelxz_N = mkN "itel-xz" ;
  itelzx_N = mkN "itel-zx" ;
  itelzy_N = mkN "itel-zy" ;
  mips_N = mkN "mips" ;

  beat_V = mkV "beat" ;
  graduate_V = mkV "graduate" ;
  live_V = mkV "live" ;
  travel_V = mkV "travel" ;
  lead_V = IrregEng.lead_V ;

  chair_V2 = mkV2 (mkV "chair") ;
  dupe_V2 = mkV2 (mkV "dupe") ;
  know_V2 = mkV2 know_V ;
  run_V2 = mkV2 run_V ;
  attend_V2 = mkV2 (mkV "attend") ;
  remove_V2 = mkV2 (mkV "remove") ;

  contribute_V3to = dirV3 (mkV "contribute") to_Prep ;

  start_VV = mkVV (mkV "start");

  see_VS = mkVS see_V ;

  really_AdA = ss "really" ;

  late_Adv = ss "late" ;

}