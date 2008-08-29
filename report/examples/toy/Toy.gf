abstract Toy = { 

cat S ; RS ; VP ; NP ; Det ; CN ; 

cat V ; V2 ; N ; N2 ; PN ;

fun PredVP : NP -> VP -> S ;

fun RelVP : VP -> RS ;

fun UseV : V -> VP ;
fun ComplV2 : V2 -> NP -> VP ;

fun DetCN : Det -> CN -> NP ;
fun Everyone : NP ;
fun Someone : NP ;
fun UsePN : PN -> NP ;

fun Every : Det ;
fun A : Det ;

fun UseN : N -> CN ;
fun ComplN2 : N2 -> NP -> CN ;
fun RelCN : CN -> RS -> CN ;

fun Walk_V : V ;
fun Love_V2 : V2 ;
fun Man_N, Woman_N, Burger_N : N ;
fun Owner_N2 : N2 ;

fun John_PN, Bill_PN, Mary_PN : PN ;

}