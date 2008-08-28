abstract Toy = { 

cat S ; RS ; VP ; NP ; Det ; CN ; 

cat V ; V2 ; N ; N2 ; PN ;

fun PredVP : NP -> VP -> S ;

fun RelVP : VP -> RS ;

fun ComplV2 : V2 -> NP -> VP ;
fun UseV : V -> VP ;

fun DetCN : Det -> CN -> NP ;
fun Everyone : NP ;
fun Someone : NP ;
fun UsePN : PN -> NP ;

fun Every : Det ;
fun A : Det ;

fun UseN : N -> CN ;
fun ComplN2 : N2 -> NP -> CN ;
fun RelCN : CN -> RS -> CN ;

fun walk_V : V ;
fun love_V2 : V2 ;
fun man_N, woman_N, bar_N : N ;
fun owner_N2 : N2 ;

fun john_PN, bill_PN, mary_PN : PN ;

}