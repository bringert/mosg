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
      
  lin Walk = { s = "walks" } ;
  lin Love = { s = "loves" } ;
  lin Eat = { s = "eats" } ;
      
  lin Man = { s = "man" } ;
  lin Woman = { s = "woman" } ;
  lin Burger = { s = "burger" } ;

  lin Owner = { s = "owner" ++ "of" } ;

  lin John = { s = "John" } ;
  lin Bill = { s = "Bill" } ;
  lin Mary = { s = "Mary" } ;

}