concrete ToyEng of Toy = {

  lincat S, RS, VP, NP, Det, CN = { s : Str } ;
  lincat V, V2, N, N2, PN = { s : Str } ;
	 
  lin PredVP np vp = { s = np.s ++ vp.s } ;
      
  lin RelVP vp = { s = "who" ++ vp.s } ;
      
  lin UseV v = { s = v.s } ;
  lin ComplV2 v2 np = { s = v2.s ++ np.s } ;
      
  lin DetCN det cn = { s = det.s ++ cn.s } ;
  lin Everyone = { s = "everyone" } ;
  lin Someone = { s = "someone" } ;
  lin UsePN pn = { s = pn.s } ;
    
  lin Every = { s = "every" } ;
  lin A = { s = "a" } ;

  lin UseN n = { s = n.s } ;
  lin ComplN2 n2 np = { s = n2.s ++ np.s } ;
  lin RelCN cn rs = { s = cn.s ++ rs.s} ;
      
  lin Walk_V = { s = "walks" } ;
  lin Love_V2 = { s = "loves" } ;
  lin Eat_V2 = { s = "eats" } ;
      
  lin Man_N = { s = "man" } ;
  lin Woman_N = { s = "woman" } ;
  lin Burger_N = { s = "burger" } ;

  lin Owner_N2 = { s = "owner" ++ "of" } ;

  lin John_PN = { s = "John" } ;
  lin Bill_PN = { s = "Bill" } ;
  lin Mary_PN = { s = "Mary" } ;

}