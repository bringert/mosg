abstract Core = 
  Grammar - 
   [
-- Phrase
    UttImpSg, UttImpPl, UttImpPol,
    UttIP, UttIAdv, UttNP, UttAdv, UttVP,
-- Structural
    part_Prep, possess_Prep
   ],
   Time

** {

flags startcat = Utt;

fun nobody_NP : NP ;

fun anybody_NP : NP ;

fun no_Det : Det ;

fun both_Det : Det ;

fun a8few_Det : Det ;

fun any_Predet : Predet ;

fun another_Predet : Predet ;

fun exactly_AdN : AdN ;

fun at8most_AdN : AdN ;

fun at8least_AdN : AdN ;

}