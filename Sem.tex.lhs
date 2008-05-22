% -*- Mode: LaTeX; coding:utf-8 -*-

\documentclass{article}

\usepackage[pdftex,bookmarks,unicode]{hyperref}

%include polycode.fmt

%if style /= newcode
%format <*>          = "\varoast"
%format &&&          = "\land"
%format |||          = "\lor"
%format ==>          = "\Rightarrow"
%format ===          = "="
%format =/=          = "\neq"
% FIXME: need better <=*=> symbol
%format <=*=>        = "\bothways"
%format thereIs      = "\exists"
%format forAll       = "\forall"
%format neg          = "\lnot"
%endif

\newcommand{\bothways}{\ensuremath{\mathaccent\varoast\longleftrightarrow}} 

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

> {-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
> module Sem where

> import GSyntax
> import FOL
> import Inter
> import Input

> import Control.Applicative (Applicative(..))
> import Control.Monad
> import Data.Traversable (traverse)
> import Data.Char

%endif

\section{Interpretation Rules}

\subsection{Text: Texts}

> iText :: GText -> [Input]
> iText (GTNoPunct phr)        = iPhr phr
> iText (GTExclMark  phr text) = pure twoStatements <*> iPhr phr <*> iText text
> iText (GTFullStop  phr text) = pure twoStatements <*> iPhr phr <*> iText text
> iText (GTQuestMark phr GTEmpty) = pure toYNQuest <*> iPhr phr
> iText GTEmpty = pure (Statement true)

%if unhandled
> iText text = unhandled "iText" text
%endif

\subsection{Phr: Phrases}

> iPhr :: GPhr -> [Input]
> iPhr (GPhrUtt GNoPConj utt GNoVoc) = iUtt utt 

%if unhandled
> iPhr phr = unhandled "iPhr" phr
%endif

\subsection{Utt: Utterances}

> iUtt :: GUtt -> [Input]
> iUtt (GUttS s) = pure Statement <*> retrieve (iS s)
> iUtt (GUttQS qs) = iQS qs

%if unhandled
> iUtt utt = unhandled "iUtt" utt
%endif

\subsection{QS: Questions}

Ignores tense and anteriority for now.

> iQS :: GQS -> [Input]
> iQS (GUseQCl tense ant pol qcl) = pure (\p c -> mapInput p c) <*> iPol pol <*> iQCl qcl

Uncontracted negated question clause (English only).

> iQS (GUncNegQCl tense ant qcl) = pure (mapInput neg) <*> iQCl qcl

\subsection{QCl: Question clauses}

> iQCl :: GQCl -> [Input]

``does John walk''

> iQCl (GQuestCl cl)          = pure YNQuest <*> retrieve (iCl cl)

``whom does John love''

> iQCl (GQuestSlash ip slash) = pure ($) <*> iIP ip <*> retrieve (iSlash slash)

% Fool highlighting: $
``who walks''

> iQCl (GQuestVP ip vp)       = pure ($) <*> iIP ip <*> retrieve (iVP vp)

``which houses are there''

> iQCl (GExistIP ip)          = pure ($ (\x -> true)) <*> iIP ip

%if unhandled
> iQCl qcl = unhandled "iQcl" qcl
%endif

% Fool highlighting: $

\subsection{IP: Interrogative Pronouns}

> iIP :: GIP -> [(Exp -> Prop) -> Input]

``which man''

> iIP (GIDetCN idet GNoNum GNoOrd cn) = pure (iIDet idet) <*> retrieve (iCN cn)
> iIP GwhatSg_IP = pure (\u -> WhQuest u)
> iIP GwhoSg_IP  = pure (\u -> WhQuest u)

%if unhandled
> iIP ip = unhandled "iIP" ip
%endif

\subsection{IDet: Interrogative Determiners}

> iIDet :: GIDet -> (Exp -> Prop) -> (Exp -> Prop) -> Input
> iIDet Ghow8many_IDet = (\u v -> CountQuest (\x -> u x &&& v x))
> iIDet GwhichSg_IDet  = (\u v -> WhQuest (\x -> u x &&& v x))

%if unhandled
> iIDet idet = unhandled "iIDet" idet
%endif

\subsection{S: Declarative Sentences}

Declarative sentences are interpreted as propositions.

> iS :: GS -> I Prop
> iS (GConjS conj ss) = pure foldr1 <*> iConj conj <*> iListS ss
> iS (GDConjS dconj ss) = pure foldr1 <*> iDConj dconj <*> iListS ss

Ignores tense and anteriority for now.

> iS (GUseCl tense ant pol cl) = iPol pol <*> iCl cl

Uncontracted negated declarative clause (English only),
e.g. ``John does not run''.
Ignores tense and anteriority for now.

> iS (GUncNegCl tense ant cl) = fmap neg (iCl cl)

Adverbial phrase modifying a sentence.
This uses a special |iAdv| version.

> iS (GAdvS adv s) = iAdv_S adv <*> iS s

A list of sentences is interpreted as a list of propositions.
Each sentence is a scope island.

> iListS :: GListS -> I [Prop]
> iListS (GListS ss) = traverse (reset . iS) ss

\subsection{Cl: Declarative Clauses}

Declarative clauses are interpreted as propositions.

> iCl :: GCl -> I Prop

Verb phrase predication, e.g. ``John sleeps''.
Either the NP or the VP can have scope priority

> iCl (GPredVP np vp) = iNP np <=*=> iVP vp

Cleft constructions, e.g. ``it is John who sleeps''.

> iCl (GCleftNP np rs) = iNP np <*> iRS rs

Existential (FIXME: what is this construction called) e.g. ``there is a house''.

> iCl (GExistNP np) = iNP np <*> pure (\x -> true)

%if unhandled
> iCl cl = unhandled "iCl" cl
%endif

\subsection{Pol: Polarity}

Polarity is straightforwardly interpreted as a function over
propositions.

> iPol :: Applicative f => GPol -> f (Prop -> Prop)
> iPol GPPos = pure id
> iPol GPNeg = pure neg

\subsection{RS: Relative Sentences}

Relative clases are interpreted as predicates.
For now we ignore the tense and anteriority.

> iRS :: GRS -> I (Exp -> Prop)
> iRS (GUseRCl tense ant pol rcl) = pure (.) <*> iPol pol <*> iRCl rcl

Uncontracted negated relative clause (English only), e.g. ``that does not sleep''.

> iRS (GUncNegRCl tense ant rcl) = pure (neg .) <*> iRCl rcl

\subsection{RCl: Relative Clauses}

Relative clauses are interpreted as predicates.

> iRCl :: GRCl -> I (Exp -> Prop)

Such-that construction, using a declarative clause as a relative
clause, e.g. ``such that a woman sleeps''.
This is mostly useful when there are anaphoric references
in the relative clause.

> iRCl (GRelCl cl) = pure (\i x -> i) <*> iCl cl

Forms a relative clause from a relative pronoun and a clause missing
a noun phrase, e.g. ``which a woman loves''.

> iRCl (GRelSlash rp slash) = iRP rp <*> iSlash slash

Relative clause with realtive pronoun and verb phrase, e.g. ``that
sleeps''.

> iRCl (GRelVP rp vp) = iRP rp <*> iVP vp

%if unhandled
> iRCl rcl = unhandled "iRCl" rcl
%endif

\subsection{RP: Relative Pronouns}

Relative pronouns are interpreted as predicate modifiers.

> iRP :: GRP -> I ((Exp -> Prop) -> (Exp -> Prop))

e.g. ``a part of which''

> iRP (GFunRP prep np rp) = pure (\pi ni ri u x -> ni (\y -> (ri u) y &&& pi (\v -> v y) x))
>                          <*> iPrep prep <*> iNP np <*> iRP rp

Relative pronoun, ``that'', ``which'', ``whose''.

> iRP GIdRP = pure (\u -> u)

Relative pronoun for animates (English only), ``who''.

> iRP Gwho_RP = pure (\u -> u)


\subsection{Slash: Clauses Missing NP}

Clause missing NP, S/NP in GPSG.

> iSlash :: GSlash -> I (Exp -> Prop)

``(which) a woman kills in Paris''

> iSlash (GAdvSlash slash adv) = iAdv adv <*> iSlash slash

``(which) a woman kills''

> iSlash (GSlashV2 np v2) = pure (\ni vi x -> ni (vi (\u -> u x)))
>                              <*> iNP np <*> iV2 v2

%if unhandled
> iSlash slash = unhandled "iSlash" slash
%endif

\subsection{Conj, DConj: Conjunctions}

Conjuctions are used in several places in the grammar, for example
on noun phrases, adjectival phrases and sentences.

> iConj :: GConj -> I (Prop -> Prop -> Prop)

``and'', interpreted as logical and.

> iConj Gand_Conj = pure (&&&)

``or'', interpreted as inclusive or.

> iConj Gor_Conj = pure (|||)

Two-part conjuctions.

> iDConj :: GDConj -> I (Prop -> Prop -> Prop)

``both ... and ...'', intepreted as and.

> iDConj Gboth7and_DConj = pure (&&&)

``either ... or ...'', interpreted as exclusive or.

> iDConj Geither7or_DConj = pure (\p q -> (p &&& neg q) ||| (neg p &&& q))


\subsection{VP: Verb Phrases}

> iVP :: GVP -> I (Exp -> Prop)

``sleeps with a woman''

> iVP (GAdvVP vp adv) = iAdv adv <*> iVP vp

``kills John''

> iVP (GComplV2 v2 np) = iV2 v2 <*> iNP np

``gives a dog to Mary''

> iVP (GComplV3 v3 np1 np2) = iV3 v3 <*> iNP np1 <*> iNP np2

``is killed''

> iVP (GPassV2 v2) = pure (\i x -> thereIs (\y -> i (\u -> u x) y)) <*> iV2 v2

``kills itself''

> iVP (GReflV2 v2) = pure (\i x -> i (\u -> u x) x) <*> iV2 v2

``is beautiful''

> iVP (GUseComp comp) = iComp comp

``sleeps''

> iVP (GUseV v) = iV v

``is sleeping''

> iVP (GProgrVP vp) = iVP vp

%if unhandled
> iVP vp = unhandled "iVP" vp
%endif

\subsection{Comp: Complement of Copula}

Complement of copula.

> iComp :: GComp -> I (Exp -> Prop)

Adjectival phrase complement, e.g. in ``(John is) very warm''.

> iComp (GCompAP ap) = iAP ap

Prepositional phrase complement, e.g. in ``(John is) in Paris''.

> iComp (GCompAdv adv) = iAdv adv <*> pure (\x -> true)

Noun phrase complemented, e.g. in ``(John is) a man''.
The complement is a scope island, to get rid of the
reading of sentences such as ``every dog is an animal'' 
where they are all the same individual.

> iComp (GCompNP np) = reset (pure (\ni x -> ni (\y -> x === y)) <*> iNP np)


\subsection{NP: Noun Phrases}

> iNP :: GNP -> I ((Exp -> Prop) -> Prop)

Noun phrase modified by an adverbial phrase,
e.g. ``Paris at midnight''.

> iNP (GAdvNP np adv) = pure (.) <*> iNP np <*> iAdv adv

Noun phrase conjunction, e.g. ``John and a man''.

> iNP (GConjNP conj nps)   = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iConj conj   <*> iListNP nps
> iNP (GDConjNP dconj nps) = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iDConj dconj <*> iListNP nps

Noun phrase formation from determiner and common noun,
e.g. ``every man''.
Note that there is no storage or anything like that here.
Instead the determiners use control operators to move
the quantifiers to the top level of the nearest enclosing scope
island. 

> iNP (GDetCN det cn) = iDet det <*> iCN cn

``only the men''. FIXME: what should we do about predet + plural?

> -- iNP (GPredetNP predet np) = (iPredet predet) (iNP np)

A noun phrase modified by a passive voice transitive verb, 
e.g. ``a woman killed''.

> iNP (GPPartNP np v2) = pure (\ni vi u -> ni (\x -> u x &&& thereIs
>                                (\y -> vi (\v -> v x) y))) <*> iNP np <*> iV2 v2

A proper name used as a noun phrase, e.g. ``John''.

> iNP (GUsePN pn) = pure (\i u -> u i) <*> iPN pn

``everybody'', ``everything''

> iNP Geverybody_NP = cont (\c -> forAll (\x -> c (\u -> u x)))
> iNP Geverything_NP = cont (\c -> forAll (\x -> c (\u -> u x)))

``somebody'', ``something''

> iNP Gsomebody_NP = cont (\c -> thereIs (\x -> c (\u -> u x)))
> iNP Gsomething_NP = cont (\c -> thereIs (\x -> c (\u -> u x)))

``nobody'', ``nothing''

> iNP Gnobody_NP = cont (\c -> neg (thereIs (\x -> c (\u -> u x))))
> iNP Gnothing_NP = cont (\c -> neg (thereIs (\x -> c (\u -> u x))))

%if unhandled
> iNP np = unhandled "iNP" np
%endif

> iListNP :: GListNP -> I [(Exp -> Prop) -> Prop]
> iListNP (GListNP nps) = traverse iNP nps

\subsection{CN: Common Nouns}

> iCN :: GCN -> I (Exp -> Prop)

Common noun modified by an adjectival phrase, e.g. ``beautiful woman''.

> iCN (GAdjCN ap cn) = pure (\ai ci x -> ai x &&& ci x) <*> iAP ap <*> iCN cn

Common noun modified by an adverbial phrase, e.g. ``woman with a dog''.

> iCN (GAdvCN cn adv) = iAdv adv <*> iCN cn

Apposition of a common noun and a noun phrase,
e.g. ``king John''. This produces some unlikely sounding 
constructions.

> iCN (GApposCN cn np) = pure (\ni ci x -> ni (x ===) &&& ci x) <*> iNP np <*> iCN cn

Complementation of a two-place noun, e.g. ``owner of a dog''.

> iCN (GComplN2 n2 np) = iN2 n2 <*> iNP np

Common noun modified by a relative clause, e.g. ``man who sleeps''.
Relative clauses are scope islands.

> iCN (GRelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> reset (iRS rs)

A noun used as a common noun, e.g. ``dog''.

> iCN (GUseN n) = iN n

A two-place noun used without a complement, e.g. ``owner''.

> iCN (GUseN2 n2) = pure (\ni x -> thereIs (\y -> ni (\u -> u y) x)) <*> iN2 n2

A three-place noun used without a complement, e.g. ``distance''.

> iCN (GUseN3 n3) = pure (\ni x -> thereIs (\y -> (thereIs (\z -> ni (\u -> u y) (\v -> v z) x)))) <*> iN3 n3

Compound common noun, e.g. ``Labour MP''.
The interpretation below is rather silly. For example,
``Labour MP'' is interpreted as $labour(X) \land mp(X)$, but the person is not the
party.

> iCN (GCompoundCN cn1 cn2) = pure (\ci1 ci2 x -> ci1 x &&& ci2 x) <*> iCN cn1 <*> iCN cn2

%if unhandled
> iCN cn = unhandled "iCN" cn
%endif

\subsection{Det: Determiners}


> iDet :: GDet -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)

A plural determiner, with a quantifier, a cardinal number
and an ordinal, e.g. ``the two best''.
FIXME: does this mean more than one?
FIXME: wrong, indef pl should be universal as subject, existential as object

> iDet (GDetPl quant num ord) = pure (\qi ni oi u v -> ni (qi (oi u) v) (oi u) v) <*> iQuant_Pl quant <*> iNum num <*> iOrd ord

A singular determiner with a quantifier and an ordinal, e.g.
``the second''.

> iDet (GDetSg quant ord) = pure (.) <*> iQuant_Sg quant <*> iOrd ord

``every''

> iDet Gevery_Det = cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))

Negated existential quantifier, ``no''.

> iDet Gno_Det = cont (\c -> neg (thereIs (\x -> c (\u v -> u x &&& v x))))

FIXME: does this mean more than one?

``some'' + plural

> iDet GsomePl_Det = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

``some'' + singular. Same as |IndefArt|.

> iDet GsomeSg_Det = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

``neither''. This is interpreted as meaning that there are exactly
two individuals with the first property, and they both lack 
the second property.

> iDet Gneither_Det = cont (\c -> thereIs (\x -> thereIs (\y -> forAll (\z -> c (\u v -> u x &&& neg (v x) &&& u y &&& neg (v y) &&& x =/= y &&& (u z ==> (z === x ||| z === y)))))))

%if unhandled
> iDet det = unhandled "iDet" det
%endif

Interpretation of quantifiers in plural positions.

> iQuant_Pl :: GQuant -> I ((Exp -> Prop) -> (Exp -> Prop) -> ((Exp -> Prop) -> Prop) -> Prop)

``(men)''.

> iQuant_Pl GIndefArt = pure (\u v n -> n (\x -> u x &&& v x))

FIXME: universal as subject, existential in object position?

> iQuant_Pl GMassDet = pure (\u v n -> n (\x -> u x &&& v x))

%if unhandled
> iQuant_Pl quant = unhandled "iQuant_Pl" quant
%endif

Interpretation of quantifiers in singular positions.

> iQuant_Sg :: GQuant -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)

``the (man)''

> iQuant_Sg GDefArt = cont (\c -> thereIs (\x -> forAll (\y -> c (\u v -> u x &&& v x &&& (u y ==> y === x)))))

``a (man''

> iQuant_Sg GIndefArt = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

``John's (dog)''.

FIXME: Should this really allow more than one? Now ``john's dog'' allows john to have several dogs.

> iQuant_Sg (GGenNP np) = cont (\c -> thereIs (\x -> c (\ni u v -> u x &&& v x &&& ni (\y -> of_Pred y x)))) <*> iNP np

``water'' FIXME: universal as subject, existential in object position?

> iQuant_Sg GMassDet = cont (\c -> forAll (\x -> c (\u v -> u x &&& v x)))

%if unhandled
> iQuant_Sg quant = unhandled "iQuant_Sg" quant
%endif

\subsection{Ord: Ordinal Number}

Ordinals and superlatives.

> iOrd :: GOrd -> I ((Exp -> Prop) -> (Exp -> Prop))

No ordinal or superlative.

> iOrd GNoOrd = pure id

Superlative adjective, e.g. ``largest''.

> iOrd (GOrdSuperl a) = pure (\comp u x -> u x &&& forAll (\y -> u y ==> comp ($ y) x)) <*> iA_comparative a
% Fool highlighting: $

%if unhandled
> iOrd ord = unhandled "iOrd" ord
%endif

\subsection{Num: Cardinal Number}

Cardinal numbers.

> iNum :: GNum -> I ((((Exp -> Prop) -> Prop) -> Prop) -> (Exp -> Prop) -> (Exp -> Prop) -> Prop)

No cardinality.

FIXME: wrong, indef pl without num should be universal as subject, existential as object

> iNum GNoNum = pure (\q u v -> forAll (\x -> u x ==> v x) &&& thereIs (\x -> thereIs (\y -> x =/= y &&& q (\p -> p x &&& p y))))

Cardinal number with digits.

> iNum (GNumDigits ds) = pure (\di q u v -> di q) <*> iInt (iDigits ds)

Cardinal number with words.

> iNum (GNumNumeral num) = pure (\di q u v -> di q) <*> iInt (iNumeral num)

Cardinal modified by a numeral-modifying adjective, e.g. ``almost five''.

> --iNum (GAdNum adn num) = iAdN adn <*> iNum num

%if unhandled
> iNum num = unhandled "iNum" num
%endif

\subsection{AP: Adjectival Phrases}

Adjectival phrases are interpreted as one-place predicates.

> iAP :: GAP -> I (Exp -> Prop)

Complementation of a two-place adjective, e.g. ``equivalent to a dog''.

> iAP (GComplA2 a2 np) = iA2 a2 <*> iNP np

Adjectival phrase conjunction.

> iAP (GConjAP conj aps)   = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iConj conj   <*> iListAP aps
> iAP (GDConjAP dconj aps) = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iDConj dconj <*> iListAP aps

Positive form of an adjective, e.g. ``big''.

> iAP (GPositA a) = iA a

Comparative form of an adjective, e.g. ``bigger''.

> iAP (GComparA a np) = iA_comparative a <*> iNP np

Reflexive use of a two-place adjective, e.g. ``equivalent to itself''.

> iAP (GReflA2 a2) = pure (\ia x -> ia (\u -> u x) x) <*> iA2 a2

%if unhandled
> iAP ap = unhandled "iAP" ap
%endif 

> iListAP :: GListAP -> I [Exp -> Prop]
> iListAP (GListAP aps) = traverse iAP aps


\subsection{Adv: Adverbial Phrases}

Adverbial phrases that can modify NP, CN, VP.

> iAdv :: GAdv -> I ((Exp -> Prop) -> (Exp -> Prop))

Adverbial phrase conjunction.

> iAdv (GConjAdv conj advs)   = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iConj conj   <*> iListAdv advs
> iAdv (GDConjAdv dconj advs) = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iDConj dconj <*> iListAdv advs

Prepositional phrase.

> iAdv (GPrepNP prep np) = pure (\pp u x -> u x &&& pp x) <*> (iPrep prep <*> iNP np)

Subjunctive phrase.

> iAdv (GSubjS subj s) = pure (\sui si u x -> sui si (u x)) <*> iSubj subj <*> iS s

%if unhandled
> iAdv adv = unhandled "iAdv" adv
%endif

> iListAdv :: GListAdv -> I [(Exp -> Prop) -> (Exp -> Prop)]
> iListAdv (GListAdv advs) = traverse iAdv advs

Interpretation of sentence-modifying adverbial phrases.

> iAdv_S :: GAdv -> I (Prop -> Prop)
> iAdv_S (GSubjS subj s) = iSubj subj <*> iS s

%if unhandled
> iAdv_S adv = unhandled "iAdv_S" adv
%endif

\subsection{Subj: Subjunctions}

> iSubj :: GSubj -> I (Prop -> Prop -> Prop)
> iSubj Galthough_Subj = pure (&&&)
> iSubj Gbecause_Subj = pure (==>)
> iSubj Gif_Subj = pure (==>)
> iSubj Gwhen_Subj = pure (==>)

%if unhandled
> iSubj subj = unhandled "iSubj" subj
%endif


\subsection{Lexical Categories}

Nouns, e.g. ``dog''.

> iN :: GN -> I (Exp -> Prop)
> iN n = pure (\x -> Pred (symbol n) [x])

Two-place nouns, e.g. ``owner of ...''.

> iN2 :: GN2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iN2 (GComplN3 n3 np) = iN3 n3 <*> iNP np
> iN2 n2 = pure (\o x -> o (\y -> Pred (symbol n2) [x,y]))

Three-place nouns, e.g. ``distance from ... to ...''.

> iN3 :: GN3 -> I (((Exp -> Prop) -> Prop) -> ((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iN3 n3 = pure (\u v x -> u (\y -> v (\z -> Pred (symbol n3) [x,y,z])))

Proper names, e.g. ``John''.

> iPN :: GPN -> I Exp
> iPN pn = pure (Const (symbol pn))

Adjectives, e.g. ``big''.

> iA :: GA -> I (Exp -> Prop)
> iA (GUseA2 a2) = pure (\i x -> thereIs (\y -> i (\v -> v y) x)) <*> iA2 a2
> iA a = pure (\x -> Pred (symbol a) [x])

Adjectives when used comparatively. FIXME: weird.

> iA_comparative :: GA -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iA_comparative a@(GUseA2 _) = unhandled "iA_comparative" a
> iA_comparative a = pure (\o x -> o (\y -> Pred (comparativeSymbol a) [x,y]))

Two-place adjectives, e.g. ``equivalent to ...''

> iA2 :: GA2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iA2 a2 = pure (\o x -> o (\y -> Pred (symbol a2) [x,y]))

Intransitive verbs, e.g. ``sleep''.

> iV :: GV -> I (Exp -> Prop)
> iV v = pure (\x -> Pred (symbol v) [x])

Transitive verbs, e.g. ``kill''.

> iV2 :: GV2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iV2 v2 = pure (\u x -> u (\y -> Pred (symbol v2) [x,y]))

Ditransitive verbs, e.g. ``give''.

> iV3 :: GV3 -> I (((Exp -> Prop) -> Prop) -> ((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iV3 v3 = pure (\u v x -> u (\y -> v (\z -> Pred (symbol v3) [x,y,z])))

Prepositions, e.g. ``in''.

> iPrep :: GPrep -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iPrep prep = pure (\u x -> u (\y -> Pred (symbol prep) [x,y]))

\subsection{Digits: Cardinals and Ordinals in Digits}

> iDigits :: GDigits -> Int
> iDigits = f 0
>     where f n (GIDig d) = 10 * n + iDig d
>           f n (GIIDig d ds) = f (10 * n + iDig d) ds

> iDig :: GDig -> Int
> iDig GD_0 = 0
> iDig GD_1 = 1
> iDig GD_2 = 2
> iDig GD_3 = 3
> iDig GD_4 = 4
> iDig GD_5 = 5
> iDig GD_6 = 6
> iDig GD_7 = 7
> iDig GD_8 = 8
> iDig GD_9 = 9

\subsection{Numeral: Cardinals and Ordinals in Words}

Numerals are interpreted as integers, and then handled by |iInt|.

> iNumeral :: GNumeral -> Int
> iNumeral (Gnum m) = iSub1000000 m

> iSub1000000 :: GSub1000000 -> Int
> iSub1000000 (Gpot2as3 m)    = iSub1000 m
> iSub1000000 (Gpot3 m)       = iSub1000 m * 1000
> iSub1000000 (Gpot3plus m n) = iSub1000 m * 1000 + iSub1000 n

> iSub1000 :: GSub1000 -> Int
> iSub1000 (Gpot1as2 m)    = iSub100 m
> iSub1000 (Gpot2 m)       = iSub10 m * 100
> iSub1000 (Gpot2plus m n) = iSub10 m * 100 + iSub100 n

> iSub100 :: GSub100 -> Int
> iSub100 Gpot110         = 10
> iSub100 Gpot111         = 11
> iSub100 (Gpot1to19 d)   = 10 + iDigit d
> iSub100 (Gpot0as1 m)    = iSub10 m
> iSub100 (Gpot1 d)       = iDigit d * 10
> iSub100 (Gpot1plus d n) = iDigit d * 10 + iSub10 n

> iSub10 :: GSub10 -> Int
> iSub10 Gpot01 = 1
> iSub10 (Gpot0 d) = iDigit d

> iDigit :: GDigit -> Int
> iDigit Gn2 = 2
> iDigit Gn3 = 3
> iDigit Gn4 = 4
> iDigit Gn5 = 5
> iDigit Gn6 = 6
> iDigit Gn7 = 7
> iDigit Gn8 = 8
> iDigit Gn9 = 9

\subsection{Int: Internal Integers}

Integer interpretation. Used above for letter and digit numerals.

> iInt :: Int -> I ((((Exp -> Prop) -> Prop) -> Prop) -> Prop)
> iInt 0 = pure (\q -> q (\p -> true))
> iInt n = pure (\ni q -> thereIs (\x -> ni (\r -> r (\y -> x =/= y) &&& q (\p -> p x &&& r p)))) <*> iInt (n-1)

%if style == newcode

\subsection{Utilities}


--
-- * Merging, fixing inputs
--

> twoStatements :: Input -> Input -> Input
> twoStatements (Statement p) (Statement q) = Statement (p &&& q)
> twoStatements s1 s2 = error "statement + question in input"

--
-- * Special predicates
--

> of_Pred :: Exp -> Exp -> Prop
> of_Pred x y = Pred "special_of" [x,y]

> comparativeSymbol :: GA -> String
> comparativeSymbol = ("more_" ++) . symbol

--
-- * Utilities
--

> symbol :: Show a => a -> String
> symbol = map toLower . takeWhile (/='_') . tail . head . words . show 

> unhandled :: Show a => String -> a -> b
> unhandled f x = error $ "Missing case in " ++ f ++ ": " ++ head (words (show x))

%endif


\end{document}