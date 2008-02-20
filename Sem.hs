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
    iCN cn = unhandled "iCN" cn

    iDet :: GDet -> i ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
    -- FIXME: does this mean more than one?
    -- FIXME: wrong, indef pl should be universal as subject, existential as object
    iDet (GDetPl quant GNoNum GNoOrd) = iQuant quant
    iDet (GDetSg quant GNoOrd) = iQuant quant
    iDet Gevery_Det = pure (\u v -> forAll (\x -> u x ==> v x))
    iDet Gno_Det = pure (\u v -> neg (thereIs (\x -> u x &&& v x)))
    -- FIXME: does this mean more than one?
    iDet GsomePl_Det = pure (\u v -> thereIs (\x -> u x &&& v x))
    iDet GsomeSg_Det = pure (\u v -> thereIs (\x -> u x &&& v x))
    iDet det = unhandled "iDet" det

    -- FIXME: handle this with numerals
--    iQuantSg Gone_Quant = pure (\u v -> thereIs (\x -> u x &&& v x &&& forAll (\y -> u y &&& v y ==> y === x)))

    iQuant :: GQuant -> i ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
    iQuant GDefArt = pure (\u v -> thereIs (\x -> u x &&& v x &&& forAll (\y -> u y ==> y === x)))
    iQuant GIndefArt = pure (\u v -> thereIs (\x -> u x &&& v x))
    iQuant quant = unhandled "iQuant" quant

    iAP :: GAP -> i (Exp -> Prop)
    iAP (GComplA2 a2 np) = iA2 a2 <*> iNP np
    iAP (GConjAP conj aps)   = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iConj conj   <*> iListAP aps
    iAP (GDConjAP dconj aps) = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iDConj dconj <*> iListAP aps
    iAP (GPositA a) = iA a
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
-- * Utilities
--

symbol :: Show a => a -> String
symbol = map toLower . takeWhile (/='_') . tail . head . words . show 

unhandled :: Show a => String -> a -> b
--unhandled f x = error $ "Missing case in " ++ f ++ ": " ++ show x
unhandled f x = error $ "Missing case in " ++ f ++ ": " ++ head (words (show x))