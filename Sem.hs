{-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
module Sem where

import GSyntax
import FOL
import Input

import Control.Applicative (Applicative(..))
import Data.Traversable (traverse)
import Data.Char

--
-- * No storage
--

newtype Id a = Id a

instance Functor Id where
    fmap f (Id x) = Id (f x)

instance Applicative Id where
    pure x = Id x
    Id f <*> Id x = Id (f x)

instance Inter Id

retrieveId :: Id a -> [a]
retrieveId (Id x) = [x]

--
-- * Interpretation
--

class Applicative i => Inter i where

--
-- Syntactic
--

    iUtt :: GUtt -> InputI i
    iUtt (GUttS s) = StatementI (iS s)
    iUtt (GUttQS qs) = QuestionI (iQS qs)
    iUtt utt = unhandled "iUtt" utt

    iQS :: GQS -> QuestI i
    -- FIXME: ignores tense and anteriority
    iQS (GUseQCl _ _ GPPos qcl) = iQCl qcl
    iQS (GUseQCl _ _ GPNeg qcl) = negQuestI (iQCl qcl)
    -- english only
    iQS (GUncNegQCl _ _ qcl) = negQuestI (iQCl qcl)

    iQCl :: GQCl -> QuestI i
    iQCl (GQuestCl cl)          = YNQuestI (iCl cl)
    iQCl (GQuestSlash ip slash) = let (i,q) = iIP ip in q (i <*> iSlash slash)
    iQCl (GQuestVP ip vp)       = let (i,q) = iIP ip in q (i <*> iVP vp)
    iQCl (GExistIP ip)          = let (i,q) = iIP ip in q (i <*> pure (\x -> true))
    iQCl qcl = unhandled "iQcl" qcl

    iIP :: GIP -> (i ((Exp -> Prop) -> (Exp -> Prop)), 
                   i (Exp -> Prop) -> QuestI i)
    iIP (GIDetCN idet GNoNum GNoOrd cn) = let (d,q) = iIDet idet in (d <*> iCN cn, q)
    iIP GwhatSg_IP = (pure (\u -> u), WhQuestI)
    -- FIXME: person(x)
    iIP GwhoSg_IP  = (pure (\u -> u), WhQuestI)
    iIP ip = unhandled "iIP" ip

    iIDet :: GIDet -> (i ((Exp -> Prop) -> (Exp -> Prop) -> (Exp -> Prop)),
                       i (Exp -> Prop) -> QuestI i)
    iIDet Ghow8many_IDet = (pure (\u v -> (\x -> u x &&& v x)), CountQuestI)
    iIDet GwhichSg_IDet  = (pure (\u v -> (\x -> u x &&& v x)), WhQuestI)
    iIDet idet = unhandled "iIDet" idet

    iS :: GS -> i Prop
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

    iListS :: GListS -> i [Prop]
    iListS (GListS ss) = traverse iS ss

    iRS :: GRS -> i (Exp -> Prop)
    -- FIXME: ignores tense and anteriority
    iRS (GUseRCl _ _ GPPos rcl) = iRCl rcl
    iRS (GUseRCl _ _ GPNeg rcl) = pure (\i x -> neg (i x)) <*> iRCl rcl
    -- english only
    iRS (GUncNegRCl _ _ rcl) = pure (\i x -> neg (i x)) <*> iRCl rcl

    iRCl :: GRCl -> i (Exp -> Prop)
    -- WEIRD: such that a woman sleeps
    iRCl (GRelCl cl) = pure (\i x -> i) <*> iCl cl
    -- which a woman kills
    iRCl (GRelSlash rp slash) = iRP rp <*> iSlash slash
    -- that sleeps
    iRCl (GRelVP rp vp) = iRP rp <*> iVP vp
    iRCl rcl = unhandled "iRCl" rcl

    iRP :: GRP -> i ((Exp -> Prop) -> (Exp -> Prop))
    -- a woman of which
    iRP (GFunRP prep np rp) = pure (\pi ni ri u x -> ni (\y -> (ri u) y &&& pi (\v -> v y) x))
                              <*> iPrep prep <*> iNP np <*> iRP rp
    iRP GIdRP = pure (\u -> u)
    -- FIXME: person(x)
    iRP Gwho_RP = pure (\u -> u)

    iSlash :: GSlash -> i (Exp -> Prop)
    -- (which) a woman kills in Paris
    iSlash (GAdvSlash slash adv) = iAdv adv <*> iSlash slash
    -- (which) a woman kills
    iSlash (GSlashV2 np v2) = pure (\ni vi x -> ni (vi (\u -> u x)))
                              <*> iNP np <*> iV2 v2
    iSlash slash = unhandled "iSlash" slash

    iConj :: GConj -> i (Prop -> Prop -> Prop)
    iConj Gand_Conj = pure (&&&)
    iConj Gor_Conj = pure (|||)

    iDConj :: GDConj -> i (Prop -> Prop -> Prop)
    iDConj Gboth7and_DConj = pure (&&&)
    -- FIXME: xor (nequiv)
    iDConj Geither7or_DConj = pure (|||)

    iCl :: GCl -> i Prop
    iCl (GPredVP np vp) = iNP np <*> iVP vp
    iCl (GCleftNP np rs) = iNP np <*> iRS rs
    iCl (GExistNP np) = iNP np <*> pure (\x -> true)
    iCl cl = unhandled "iCl" cl

    iVP :: GVP -> i (Exp -> Prop)
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

    iComp :: GComp -> i (Exp -> Prop)
    iComp (GCompAP ap) = iAP ap
    iComp (GCompAdv adv) = iAdv adv <*> pure (\x -> true)
    iComp (GCompNP np) = pure (\ni x -> ni (\y -> x === y)) <*> iNP np

    iNP :: GNP -> i ((Exp -> Prop) -> Prop)
    iNP (GAdvNP np adv) = pure (.) <*> iNP np <*> iAdv adv

    iNP (GConjNP conj nps)   = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iConj conj   <*> iListNP nps
    iNP (GDConjNP dconj nps) = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iDConj dconj <*> iListNP nps
    -- *** here's where the storage should happen
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

    iListNP :: GListNP -> i [(Exp -> Prop) -> Prop]
    iListNP (GListNP nps) = traverse iNP nps

    iCN :: GCN -> i (Exp -> Prop)
    -- beautiful woman
    iCN (GAdjCN ap cn) = pure (\ai ci x -> ai x &&& ci x) <*> iAP ap <*> iCN cn
    -- woman with a woman
    iCN (GAdvCN cn adv) = iAdv adv <*> iCN cn
    -- FIXME: weird: a woman Paris
    iCN (GApposCN cn np) = pure (\ni ci x -> ni (x ===) &&& ci x) <*> iNP np <*> iCN cn
    iCN (GComplN2 n2 np) = iN2 n2 <*> iNP np
    iCN (GRelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs
    iCN (GUseN n) = iN n
    iCN (GUseN2 n2) = pure (\ni x -> thereIs (\y -> ni (\u -> u y) x)) <*> iN2 n2
    iCN (GUseN3 n3) = pure (\ni x -> thereIs (\y -> (thereIs (\z -> ni (\u -> u y) (\v -> v z) x)))) <*> iN3 n3
    -- FIXME: this produces some odd predicates, 
    -- e.g. "labour mp" -> "labour(X) & mp(X)",
    -- but the person is not the party
    iCN (GCompoundCN cn1 cn2) = pure (\ci1 ci2 x -> ci1 x &&& ci2 x) <*> iCN cn1 <*> iCN cn2
    iCN cn = unhandled "iCN" cn

    iDet :: GDet -> i ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
    -- FIXME: does this mean more than one?
    -- FIXME: wrong, indef pl should be universal as subject, existential as object
    iDet (GDetPl quant num ord) = pure (\qi ni oi u v -> ni (qi (oi u) v) (oi u) v) <*> iQuant_Pl quant <*> iNum num <*> iOrd ord
    iDet (GDetSg quant ord) = pure (.) <*> iQuant_Sg quant <*> iOrd ord
    iDet Gevery_Det = pure (\u v -> forAll (\x -> u x ==> v x))
    iDet Gno_Det = pure (\u v -> neg (thereIs (\x -> u x &&& v x)))
    -- FIXME: does this mean more than one?
    iDet GsomePl_Det = pure (\u v -> thereIs (\x -> u x &&& v x))
    iDet GsomeSg_Det = pure (\u v -> thereIs (\x -> u x &&& v x))
    iDet det = unhandled "iDet" det

    iNum :: GNum -> i ((((Exp -> Prop) -> Prop) -> Prop) -> (Exp -> Prop) -> (Exp -> Prop) -> Prop)
    -- FIXME: wrong, indef pl without num should be universal as subject, existential as object
    iNum GNoNum = pure (\q u v -> forAll (\x -> u x ==> v x) &&& thereIs (\x -> thereIs (\y -> q (\p -> p x &&& p y))))
    iNum (GNumDigits ds) = pure (\di q u v -> di q) <*> iInt (iDigits ds)
    iNum (GNumNumeral num) = pure (\di q u v -> di q) <*> iInt (iNumeral num)
--    iNum (GAdNum adn num) = iAdN adn <*> iNum num
    iNum num = unhandled "iNum" num

    -- FIXME: they should be unique
    iInt :: Int -> i ((((Exp -> Prop) -> Prop) -> Prop) -> Prop)
    iInt 0 = pure (\q -> q (\p -> true))
    iInt n = pure (\ni q -> thereIs (\x -> ni (\r -> q (\p -> p x &&& r p) &&& r (\y -> x =/= y)))) <*> iInt (n-1)

--    iAdN :: GAdN -> i ...
--    iAdN Gexactly_AdN = 
--    iAdN Gat8most_AdN = 
--    iAdN Gat8least_AdN = 

    iQuant_Pl :: GQuant -> i ((Exp -> Prop) -> (Exp -> Prop) -> ((Exp -> Prop) -> Prop) -> Prop)
    iQuant_Pl GDefArt = pure (\u v n -> n (\x -> u x &&& v x) &&& neg (thereIs (\y -> u y &&& n (\z -> y =/= z))))
    iQuant_Pl GIndefArt = pure (\u v n -> n (\x -> u x &&& v x))
--    iQuant_Pl (GGenNP np) = pure (\ni u v n -> n (\x -> u x &&& ni (\y -> of_Pred y x) &&& v x) &&& neg (thereIs (\y -> u y &&& ni (\y -> of_Pred y x) &&& n (\z -> y =/= z)))) <*> iNP np
    iQuant_Pl quant = unhandled "iQuant_Pl" quant

    iQuant_Sg :: GQuant -> i ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
    iQuant_Sg GDefArt = pure (\u v -> thereIs (\x -> u x &&& v x &&& forAll (\y -> u y ==> y === x)))
    iQuant_Sg GIndefArt = pure (\u v -> thereIs (\x -> u x &&& v x))
    -- FIXME: Should this really allow more than one? Now "john's dog runs" allows john to have several dogs.
    iQuant_Sg (GGenNP np) = pure (\ni u v -> thereIs (\x -> u x &&& v x &&& ni (\y -> of_Pred y x))) <*> iNP np
    iQuant_Sg quant = unhandled "iQuant_Sg" quant

    iOrd :: GOrd -> i ((Exp -> Prop) -> (Exp -> Prop))
    iOrd GNoOrd = pure id
    iOrd (GOrdSuperl a) = pure (\comp u x -> u x &&& forAll (\y -> u y ==> comp ($ y) x)) <*> iA_comparative a
    iOrd ord = unhandled "iOrd" ord

    iAP :: GAP -> i (Exp -> Prop)
    iAP (GComplA2 a2 np) = iA2 a2 <*> iNP np
    iAP (GConjAP conj aps)   = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iConj conj   <*> iListAP aps
    iAP (GDConjAP dconj aps) = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iDConj dconj <*> iListAP aps
    iAP (GPositA a) = iA a
    iAP (GComparA a np) = iA_comparative a <*> iNP np
    iAP (GReflA2 a2) = pure (\ia x -> ia (\u -> u x) x) <*> iA2 a2
    iAP ap = unhandled "iAP" ap

    iListAP :: GListAP -> i [Exp -> Prop]
    iListAP (GListAP aps) = traverse iAP aps

    -- Adv modifying NP, CN, VP
    iAdv :: GAdv -> i ((Exp -> Prop) -> (Exp -> Prop))
    iAdv (GConjAdv conj advs)   = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iConj conj   <*> iListAdv advs
    iAdv (GDConjAdv dconj advs) = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iDConj dconj <*> iListAdv advs
    iAdv (GPrepNP prep np) = pure (\pp u x -> u x &&& pp x) <*> (iPrep prep <*> iNP np)
    iAdv (GSubjS subj s) = pure (\sui si u x -> sui si (u x)) <*> iSubj subj <*> iS s
    iAdv adv = unhandled "iAdv" adv
    
    -- Adv modifying S
    iAdv_S :: GAdv -> i (Prop -> Prop)
    iAdv_S (GSubjS subj s) = iSubj subj <*> iS s
    iAdv_S adv = unhandled "iAdv_S" adv

    iSubj :: GSubj -> i (Prop -> Prop -> Prop)
    iSubj Galthough_Subj = pure (&&&)
    -- FIXME: is because implication?
    iSubj Gbecause_Subj = pure ((==>))
    iSubj Gif_Subj = pure (==>)
    iSubj Gwhen_Subj = pure (==>)
    -- english only
    -- FIXME: weird syntax: "therefore john sleeps , felix sleeps"
    --iSubj Gtherefore_Subj = pure (flip (==>))
    iSubj subj = unhandled "iSubj" subj

    iListAdv :: GListAdv -> i [(Exp -> Prop) -> (Exp -> Prop)]
    iListAdv (GListAdv advs) = traverse iAdv advs

--
-- Lexical 
--

    iN :: GN -> i (Exp -> Prop)
    iN n = pure (\x -> Pred (symbol n) [x])

    iN2 :: GN2 -> i (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iN2 (GComplN3 n3 np) = iN3 n3 <*> iNP np
    iN2 n2 = pure (\o x -> o (\y -> Pred (symbol n2) [x,y]))

    iN3 :: GN3 -> i (((Exp -> Prop) -> Prop) -> ((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iN3 n3 = pure (\u v x -> u (\y -> v (\z -> Pred (symbol n3) [x,y,z])))

    iPN :: GPN -> i Exp
    iPN pn = pure (Const (symbol pn))

    iA :: GA -> i (Exp -> Prop)
    iA (GUseA2 a2) = pure (\i x -> thereIs (\y -> i (\v -> v y) x)) <*> iA2 a2
    iA a = pure (\x -> Pred (symbol a) [x])

    iA_comparative :: GA -> i (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iA_comparative a@(GUseA2 _) = unhandled "iA_comparative" a
    iA_comparative a = pure (\o x -> o (\y -> Pred (comparativeSymbol a) [x,y]))

    iA2 :: GA2 -> i (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iA2 a2 = pure (\o x -> o (\y -> Pred (symbol a2) [x,y]))

    iV :: GV -> i (Exp -> Prop)
    iV v = pure (\x -> Pred (symbol v) [x])

    iV2 :: GV2 -> i (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iV2 v2 = pure (\u x -> u (\y -> Pred (symbol v2) [x,y]))

    iV3 :: GV3 -> i (((Exp -> Prop) -> Prop) -> ((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iV3 v3 = pure (\u v x -> u (\y -> v (\z -> Pred (symbol v3) [x,y,z])))

    iPrep :: GPrep -> i (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
    iPrep prep = pure (\u x -> u (\y -> Pred (symbol prep) [x,y]))

--
-- * Numbers
--

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
--unhandled f x = error $ "Missing case in " ++ f ++ ": " ++ show x
unhandled f x = error $ "Missing case in " ++ f ++ ": " ++ head (words (show x))