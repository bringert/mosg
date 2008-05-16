\documentclass{article}

%include polycode.fmt

%format <*>          = "\varoast"
%format &&&          = "\land"
%format |||          = "\lor"
%format ==>          = "\Rightarrow"
%format thereIs      = "\exists"
%format forAll       = "\forall"
%format neg          = "\lnot"


\title{First-order Logic Semantics for the 
Grammatical Framework Resource Grammar Library}

\author{
  Bj\"{o}rn Bringert \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology and University of Gothenburg\\
  \texttt{bringert{\char64}cs.chalmers.se}}

\begin{document}

\maketitle


\begin{abstract}
We present a compositional first-order logic semantics
for the Grammatical Framework resource grammar library.
\end{abstract}

\section{Introduction}



%if style == newcode

\begin{code}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
module Sem where

import GSyntax
import FOL
import Inter
import Input

import Control.Applicative (Applicative(..))
import Control.Monad
import Data.Traversable (traverse)
import Data.Char
\end{code}

%endif

\section{Interpretation Rules}

\begin{code}
iText :: GText -> [Input]
iText (GTNoPunct phr)        = iPhr phr
iText (GTExclMark  phr text) = pure twoStatements <*> iPhr phr <*> iText text
iText (GTFullStop  phr text) = pure twoStatements <*> iPhr phr <*> iText text
iText (GTQuestMark phr GTEmpty) = pure toYNQuest <*> iPhr phr
iText GTEmpty = pure (Statement true)
iText text = unhandled "iText" text

iPhr :: GPhr -> [Input]
iPhr (GPhrUtt GNoPConj utt GNoVoc) = iUtt utt 
iPhr phr = unhandled "iPhr" phr

iUtt :: GUtt -> [Input]
iUtt (GUttS s) = pure Statement <*> retrieve (iS s)
iUtt (GUttQS qs) = iQS qs
iUtt utt = unhandled "iUtt" utt
\end{code}

\subsection{Questions}

\begin{code}
iQS :: GQS -> [Input]
-- FIXME: ignores tense and anteriority
iQS (GUseQCl _ _ GPPos qcl) = iQCl qcl
iQS (GUseQCl _ _ GPNeg qcl) = pure negInput <*> iQCl qcl
-- english only
iQS (GUncNegQCl _ _ qcl) = pure negInput <*> iQCl qcl

iQCl :: GQCl -> [Input]
-- does John walk
iQCl (GQuestCl cl)          = pure YNQuest <*> retrieve (iCl cl)
-- whom does John love
iQCl (GQuestSlash ip slash) = pure ($) <*> iIP ip <*> retrieveFun (iSlash slash)
-- who walks
iQCl (GQuestVP ip vp)       = pure ($) <*> iIP ip <*> retrieveFun (iVP vp)
-- which houses are there
iQCl (GExistIP ip)          = pure ($ (\x -> true)) <*> iIP ip
iQCl qcl = unhandled "iQcl" qcl
           
iIP :: GIP -> [(Exp -> Prop) -> Input]
-- which man
iIP (GIDetCN idet GNoNum GNoOrd cn) = pure (iIDet idet) <*> retrieveFun (iCN cn)
iIP GwhatSg_IP = pure (\u -> WhQuest u)
-- FIXME: person(x)
iIP GwhoSg_IP  = pure (\u -> WhQuest u)
iIP ip = unhandled "iIP" ip

iIDet :: GIDet -> (Exp -> Prop) -> (Exp -> Prop) -> Input
iIDet Ghow8many_IDet = (\u v -> CountQuest (\x -> u x &&& v x))
iIDet GwhichSg_IDet  = (\u v -> WhQuest (\x -> u x &&& v x))
iIDet idet = unhandled "iIDet" idet
\end{code}

\subsection{Declarative Sentences and Clauses}

\begin{code}
iS :: GS -> I Prop
iS (GConjS conj ss) = pure foldr1 <*> iConj conj <*> iListS ss
iS (GDConjS dconj ss) = pure foldr1 <*> iDConj dconj <*> iListS ss
-- FIXME: ignores tense and anteriority
iS (GUseCl _ _ GPPos cl) = iCl cl
iS (GUseCl _ _ GPNeg cl) = fmap neg (iCl cl)
-- a bit odd: uses a special iAdv version
iS (GAdvS adv s) = iAdv_S adv <*> iS s
-- english only
-- FIXME: ignores tense and anteriority
iS (GUncNegCl _ _ cl) = fmap neg (iCl cl)

iListS :: GListS -> I [Prop]
iListS (GListS ss) = traverse iS ss

iCl :: GCl -> I Prop
-- Either the NP or the VP can have scope priority
iCl (GPredVP np vp) = iNP np <=*=> iVP vp
iCl (GCleftNP np rs) = iNP np <*> iRS rs
iCl (GExistNP np) = iNP np <*> pure (\x -> true)
iCl cl = unhandled "iCl" cl
\end{code}

\subsection{Relative Clauses}

\begin{code}
iRS :: GRS -> I (Exp -> Prop)
-- FIXME: ignores tense and anteriority
iRS (GUseRCl _ _ GPPos rcl) = iRCl rcl
iRS (GUseRCl _ _ GPNeg rcl) = pure (\i x -> neg (i x)) <*> iRCl rcl
-- english only
iRS (GUncNegRCl _ _ rcl) = pure (\i x -> neg (i x)) <*> iRCl rcl

iRCl :: GRCl -> I (Exp -> Prop)
-- WEIRD: such that a woman sleeps
iRCl (GRelCl cl) = pure (\i x -> i) <*> iCl cl
-- which a woman kills
iRCl (GRelSlash rp slash) = iRP rp <*> iSlash slash
-- that sleeps
iRCl (GRelVP rp vp) = iRP rp <*> iVP vp
iRCl rcl = unhandled "iRCl" rcl

iRP :: GRP -> I ((Exp -> Prop) -> (Exp -> Prop))
-- a woman of which
iRP (GFunRP prep np rp) = pure (\pi ni ri u x -> ni (\y -> (ri u) y &&& pi (\v -> v y) x))
                          <*> iPrep prep <*> iNP np <*> iRP rp
iRP GIdRP = pure (\u -> u)
-- FIXME: person(x)
iRP Gwho_RP = pure (\u -> u)
\end{code}

\subsection{Slash}

\begin{code}
iSlash :: GSlash -> I (Exp -> Prop)
-- (which) a woman kills in Paris
iSlash (GAdvSlash slash adv) = iAdv adv <*> iSlash slash
-- (which) a woman kills
iSlash (GSlashV2 np v2) = pure (\ni vi x -> ni (vi (\u -> u x)))
                              <*> iNP np <*> iV2 v2
iSlash slash = unhandled "iSlash" slash
\end{code}

\subsection{Conjunctions}

\begin{code}
iConj :: GConj -> I (Prop -> Prop -> Prop)
iConj Gand_Conj = pure (&&&)
iConj Gor_Conj = pure (|||)

iDConj :: GDConj -> I (Prop -> Prop -> Prop)
iDConj Gboth7and_DConj = pure (&&&)
-- FIXME: xor (nequiv)
iDConj Geither7or_DConj = pure (|||)
\end{code}

\subsection{Verb Phrases}

\begin{code}
iVP :: GVP -> I (Exp -> Prop)
-- sleeps with a woman
iVP (GAdvVP vp adv) = iAdv adv <*> iVP vp
-- kills Paris
iVP (GComplV2 v2 np) = iV2 v2 <*> iNP np
-- gives Paris to a woman
iVP (GComplV3 v3 np1 np2) = iV3 v3 <*> iNP np1 <*> iNP np2
-- is killed
iVP (GPassV2 v2) = pure (\i x -> thereIs (\y -> i (\u -> u x) y)) <*> iV2 v2
-- kills itself  FIXME: herself?
iVP (GReflV2 v2) = pure (\i x -> i (\u -> u x) x) <*> iV2 v2
-- is beautiful
iVP (GUseComp comp) = iComp comp
-- sleeps
iVP (GUseV v) = iV v
-- is sleeping
iVP (GProgrVP vp) = iVP vp
iVP vp = unhandled "iVP" vp

iComp :: GComp -> I (Exp -> Prop)
iComp (GCompAP ap) = iAP ap
iComp (GCompAdv adv) = iAdv adv <*> pure (\x -> true)
iComp (GCompNP np) = pure (\ni x -> ni (\y -> x === y)) <*> iNP np
\end{code}

\subsection{Noun Phrases}

\begin{code}
iNP :: GNP -> I ((Exp -> Prop) -> Prop)
iNP (GAdvNP np adv) = pure (.) <*> iNP np <*> iAdv adv

iNP (GConjNP conj nps)   = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iConj conj   <*> iListNP nps
iNP (GDConjNP dconj nps) = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iDConj dconj <*> iListNP nps
iNP (GDetCN det cn) = iDet det <*> iCN cn

-- all men
-- FIXME: what should we do about predet + plural?
-- iNP (GPredetNP predet np) = (iPredet predet) (iNP np)
-- a woman killed
iNP (GPPartNP np v2) = pure (\ni vi u -> ni (\x -> u x &&& thereIs (\y -> vi (\v -> v x) y))) <*> iNP np <*> iV2 v2
iNP (GUsePN pn) = pure (\i u -> u i) <*> iPN pn
-- FIXME: person(x) => ... ?
iNP Geverybody_NP = pure (\u -> forAll (\x -> u x))
iNP Geverything_NP = pure (\u -> forAll (\x -> u x))
-- FIXME: person(x) => ... ?
iNP Gsomebody_NP = pure (\u -> thereIs (\x -> u x))
iNP Gsomething_NP = pure (\u -> thereIs (\x -> u x))
iNP Gnobody_NP = pure (\u -> neg (thereIs (\x -> u x)))
iNP np = unhandled "iNP" np

iListNP :: GListNP -> I [(Exp -> Prop) -> Prop]
iListNP (GListNP nps) = traverse iNP nps

iCN :: GCN -> I (Exp -> Prop)
-- beautiful woman
iCN (GAdjCN ap cn) = pure (\ai ci x -> ai x &&& ci x) <*> iAP ap <*> iCN cn
-- woman with a woman
iCN (GAdvCN cn adv) = iAdv adv <*> iCN cn
-- FIXME: weird: a woman Paris
iCN (GApposCN cn np) = pure (\ni ci x -> ni (x ===) &&& ci x) <*> iNP np <*> iCN cn
iCN (GComplN2 n2 np) = iN2 n2 <*> iNP np
-- Quantifiers in a relative clause cannot outscope those outside the relative clause.
iCN (GRelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs
iCN (GUseN n) = iN n
iCN (GUseN2 n2) = pure (\ni x -> thereIs (\y -> ni (\u -> u y) x)) <*> iN2 n2
iCN (GUseN3 n3) = pure (\ni x -> thereIs (\y -> (thereIs (\z -> ni (\u -> u y) (\v -> v z) x)))) <*> iN3 n3
-- FIXME: this produces some odd predicates, 
-- e.g. "labour mp" -> "labour(X) \& mp(X)",
-- but the person is not the party
iCN (GCompoundCN cn1 cn2) = pure (\ci1 ci2 x -> ci1 x &&& ci2 x) <*> iCN cn1 <*> iCN cn2
iCN cn = unhandled "iCN" cn
\end{code}

\subsection{Determiners}

\begin{code}
iDet :: GDet -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
iDet (GDetSg quant ord) = pure (.) <*> iQuant_Sg quant <*> iOrd ord
iDet Gevery_Det = cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
iDet Gno_Det = cont (\c -> neg (thereIs (\x -> c (\u v -> u x &&& v x))))
-- FIXME: does this mean more than one?
iDet GsomePl_Det = pure (\u v -> thereIs (\x -> u x &&& v x))
-- Same as IndefArt
iDet GsomeSg_Det = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))
iDet Gneither_Det = cont (\c -> thereIs (\x -> thereIs (\y -> forAll (\z -> c (\u v -> u x &&& neg (v x) &&& u y &&& neg (v y) &&& x =/= y &&& (u z ==> (z === x ||| z === y)))))))
iDet det = unhandled "iDet" det

iQuant_Pl :: GQuant -> I ((Exp -> Prop) -> (Exp -> Prop) -> ((Exp -> Prop) -> Prop) -> Prop)
iQuant_Pl GIndefArt = pure (\u v n -> n (\x -> u x &&& v x))
-- FIXME: universal as subject, existential in object position?
iQuant_Pl GMassDet = pure (\u v n -> n (\x -> u x &&& v x))
iQuant_Pl quant = unhandled "iQuant_Pl" quant

iQuant_Sg :: GQuant -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
iQuant_Sg GDefArt = cont (\c -> thereIs (\x -> forAll (\y -> c (\u v -> u x &&& v x &&& (u y ==> y === x)))))
iQuant_Sg GIndefArt = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))
-- FIXME: Should this really allow more than one? Now "john's dog runs" allows john to have several dogs.
iQuant_Sg (GGenNP np) = cont (\c -> thereIs (\x -> c (\ni u v -> u x &&& v x &&& ni (\y -> of_Pred y x)))) <*> iNP np
-- FIXME: universal as subject, existential in object position?
iQuant_Sg GMassDet = cont (\c -> forAll (\x -> c (\u v -> u x &&& v x)))
iQuant_Sg quant = unhandled "iQuant_Sg" quant

iOrd :: GOrd -> I ((Exp -> Prop) -> (Exp -> Prop))
iOrd GNoOrd = pure id
iOrd (GOrdSuperl a) = pure (\comp u x -> u x &&& forAll (\y -> u y ==> comp ($ y) x)) <*> iA_comparative a
iOrd ord = unhandled "iOrd" ord
\end{code}

\subsection{Numerals}

\begin{code}
iNum :: GNum -> I ((((Exp -> Prop) -> Prop) -> Prop) -> (Exp -> Prop) -> (Exp -> Prop) -> Prop)
-- FIXME: wrong, indef pl without num should be universal as subject, existential as object
iNum GNoNum = pure (\q u v -> forAll (\x -> u x ==> v x) &&& thereIs (\x -> thereIs (\y -> x =/= y &&& q (\p -> p x &&& p y))))
iNum (GNumDigits ds) = pure (\di q u v -> di q) <*> iInt (iDigits ds)
iNum (GNumNumeral num) = pure (\di q u v -> di q) <*> iInt (iNumeral num)
--iNum (GAdNum adn num) = iAdN adn <*> iNum num
iNum num = unhandled "iNum" num

-- FIXME: they should be unique
iInt :: Int -> I ((((Exp -> Prop) -> Prop) -> Prop) -> Prop)
iInt 0 = pure (\q -> q (\p -> true))
iInt n = pure (\ni q -> thereIs (\x -> ni (\r -> r (\y -> x =/= y) &&& q (\p -> p x &&& r p)))) <*> iInt (n-1)
\end{code}

\subsection{Adjectival Phrases}

\begin{code}
iAP :: GAP -> I (Exp -> Prop)
iAP (GComplA2 a2 np) = iA2 a2 <*> iNP np
iAP (GConjAP conj aps)   = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iConj conj   <*> iListAP aps
iAP (GDConjAP dconj aps) = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iDConj dconj <*> iListAP aps
iAP (GPositA a) = iA a
iAP (GComparA a np) = iA_comparative a <*> iNP np
iAP (GReflA2 a2) = pure (\ia x -> ia (\u -> u x) x) <*> iA2 a2
iAP ap = unhandled "iAP" ap

iListAP :: GListAP -> I [Exp -> Prop]
iListAP (GListAP aps) = traverse iAP aps
\end{code}

\subsection{Adverbs}

\begin{code}
-- Adv modifying NP, CN, VP
iAdv :: GAdv -> I ((Exp -> Prop) -> (Exp -> Prop))
iAdv (GConjAdv conj advs)   = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iConj conj   <*> iListAdv advs
iAdv (GDConjAdv dconj advs) = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iDConj dconj <*> iListAdv advs
iAdv (GPrepNP prep np) = pure (\pp u x -> u x &&& pp x) <*> (iPrep prep <*> iNP np)
iAdv (GSubjS subj s) = pure (\sui si u x -> sui si (u x)) <*> iSubj subj <*> iS s
iAdv adv = unhandled "iAdv" adv
    
-- Adv modifying S
iAdv_S :: GAdv -> I (Prop -> Prop)
iAdv_S (GSubjS subj s) = iSubj subj <*> iS s
iAdv_S adv = unhandled "iAdv_S" adv

iSubj :: GSubj -> I (Prop -> Prop -> Prop)
iSubj Galthough_Subj = pure (&&&)
iSubj Gbecause_Subj = pure ((==>))
iSubj Gif_Subj = pure (==>)
iSubj Gwhen_Subj = pure (==>)
iSubj subj = unhandled "iSubj" subj

iListAdv :: GListAdv -> I [(Exp -> Prop) -> (Exp -> Prop)]
iListAdv (GListAdv advs) = traverse iAdv advs
\end{code}

\subsection{Lexical Categories}

\begin{code}
iN :: GN -> I (Exp -> Prop)
iN n = pure (\x -> Pred (symbol n) [x])

iN2 :: GN2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iN2 (GComplN3 n3 np) = iN3 n3 <*> iNP np
iN2 n2 = pure (\o x -> o (\y -> Pred (symbol n2) [x,y]))

iN3 :: GN3 -> I (((Exp -> Prop) -> Prop) -> ((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iN3 n3 = pure (\u v x -> u (\y -> v (\z -> Pred (symbol n3) [x,y,z])))

iPN :: GPN -> I Exp
iPN pn = pure (Const (symbol pn))

iA :: GA -> I (Exp -> Prop)
iA (GUseA2 a2) = pure (\i x -> thereIs (\y -> i (\v -> v y) x)) <*> iA2 a2
iA a = pure (\x -> Pred (symbol a) [x])

iA_comparative :: GA -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iA_comparative a@(GUseA2 _) = unhandled "iA_comparative" a
iA_comparative a = pure (\o x -> o (\y -> Pred (comparativeSymbol a) [x,y]))

iA2 :: GA2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iA2 a2 = pure (\o x -> o (\y -> Pred (symbol a2) [x,y]))

iV :: GV -> I (Exp -> Prop)
iV v = pure (\x -> Pred (symbol v) [x])

iV2 :: GV2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iV2 v2 = pure (\u x -> u (\y -> Pred (symbol v2) [x,y]))

iV3 :: GV3 -> I (((Exp -> Prop) -> Prop) -> ((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iV3 v3 = pure (\u v x -> u (\y -> v (\z -> Pred (symbol v3) [x,y,z])))

iPrep :: GPrep -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
iPrep prep = pure (\u x -> u (\y -> Pred (symbol prep) [x,y]))

\end{code}

\subsection{Numbers (merge with above?)}

\begin{code}

iDigits :: GDigits -> Int
iDigits = f 0
    where f n (GIDig d) = 10 * n + iDig d
          f n (GIIDig d ds) = f (10 * n + iDig d) ds

iDig :: GDig -> Int
iDig GD_0 = 0
iDig GD_1 = 1
iDig GD_2 = 2
iDig GD_3 = 3
iDig GD_4 = 4
iDig GD_5 = 5
iDig GD_6 = 6
iDig GD_7 = 7
iDig GD_8 = 8
iDig GD_9 = 9

iNumeral :: GNumeral -> Int
iNumeral (Gnum m) = iSub1000000 m

iSub1000000 :: GSub1000000 -> Int
iSub1000000 (Gpot2as3 m)    = iSub1000 m
iSub1000000 (Gpot3 m)       = iSub1000 m * 1000
iSub1000000 (Gpot3plus m n) = iSub1000 m * 1000 + iSub1000 n

iSub1000 :: GSub1000 -> Int
iSub1000 (Gpot1as2 m)    = iSub100 m
iSub1000 (Gpot2 m)       = iSub10 m * 100
iSub1000 (Gpot2plus m n) = iSub10 m * 100 + iSub100 n

iSub100 :: GSub100 -> Int
iSub100 Gpot110         = 10
iSub100 Gpot111         = 11
iSub100 (Gpot1to19 d)   = 10 + iDigit d
iSub100 (Gpot0as1 m)    = iSub10 m
iSub100 (Gpot1 d)       = iDigit d * 10
iSub100 (Gpot1plus d n) = iDigit d * 10 + iSub10 n

iSub10 :: GSub10 -> Int
iSub10 Gpot01 = 1
iSub10 (Gpot0 d) = iDigit d

iDigit :: GDigit -> Int
iDigit Gn2 = 2
iDigit Gn3 = 3
iDigit Gn4 = 4
iDigit Gn5 = 5
iDigit Gn6 = 6
iDigit Gn7 = 7
iDigit Gn8 = 8
iDigit Gn9 = 9

\end{code}

\subsection{Utilities}

\begin{code}

--
-- * Merging, fixing inputs
--

twoStatements :: Input -> Input -> Input
twoStatements (Statement p) (Statement q) = Statement (p &&& q)
twoStatements s1 s2 = error "statement + question in input"

--
-- * Special predicates
--

of_Pred :: Exp -> Exp -> Prop
of_Pred x y = Pred "special_of" [x,y]

comparativeSymbol :: GA -> String
comparativeSymbol = ("more_" ++) . symbol

--
-- * Utilities
--

symbol :: Show a => a -> String
symbol = map toLower . takeWhile (/='_') . tail . head . words . show 

unhandled :: Show a => String -> a -> b
unhandled f x = error $ "Missing case in " ++ f ++ ": " ++ head (words (show x))

\end{code}

\end{document}