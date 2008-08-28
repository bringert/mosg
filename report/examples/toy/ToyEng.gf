concrete ToyEng of Toy = {

lincat S, RS, VP, NP, Det, CN = { s : Str } ;
lincat V, V2, N, N2, PN = { s : Str } ;

lin PredVP np vp = { s = np.s ++ vp.s } ;

lin RelVP vp = { s = "who" ++ vp.s } ;

lin ComplV2 v2 np = { s = v2.s ++ np.s } ;
lin UseV v = { s = v.s } ;

lin DetCN det cn = { s = det.s ++ cn.s } ;
lin Everyone = { s = "everyone" } ;
lin Someone = { s = "someone" } ;
lin UsePN pn = { s = pn.s } ;

lin Every = { s = "every" } ;
lin A = { s = "a" } ;

lin UseN n = { s = n.s } ;
lin ComplN2 n2 np = { s = n2.s ++ np.s } ;
lin RelCN cn rs = { s = cn.s ++ rs.s} ;

lin walk_V = { s = "walks" } ;
lin love_V2 = { s = "loves" } ;

lin man_N = { s = "man" } ;
lin woman_N = { s = "woman" } ;
lin bar_N = { s = "bar" } ;

lin owner_N2 = { s = "owner" ++ "of" } ;

lin john_PN = { s = "John" } ;
lin bill_PN = { s = "Bill" } ;
lin mary_PN = { s = "Mary" } ;

}