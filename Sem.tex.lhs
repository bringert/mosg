% -*- Mode: LaTeX; coding:utf-8 -*-

%include semantics.fmt


\section{First-order Logic Semantics for the 
Grammatical Framework Resource Grammar Library}

%if style == newcode

> {-# OPTIONS_GHC -fwarn-incomplete-patterns -fwarn-overlapping-patterns #-}
> {-# LANGUAGE DeriveDataTypeable #-}
> module Sem (interpretText, UnhandledTree(..), tryUnhandledTree) where

> import Syntax
> import FOL
> import Inter
> import Input

> import Control.Applicative (Applicative(..))
> import Control.Monad
> import Data.Traversable (traverse)
> import Data.Char

> import PGF (Tree)
> import Control.Exception (throwDyn, catchDyn)
> import Data.Typeable (Typeable)

%endif


\section{Interpretation Rules}

> type I a = Cont Prop a

> type I' a = Cont Input a

> interpretText :: GText -> [Input]
> interpretText = eval . iText

\subsection{Output Language}

The target of the interpretation is lambda calculus over 
first-order logic, with some annotations that indicate the 
type of utterance.

%if style /= newcode
%include Input.lhs
%endif


\subsection{Text: Texts}

The top-level category is |Text|, a sequence of phrases with punctuation.
%
> iText :: GText -> I' Input
%
A single phrase without punctuation is accepted.
%
> iText (GTNoPunct phr)        = iPhr phr
%
Two phrases with a full stop or an exclamation mark in between are interpreted
as a conjunction of two statements. If they are not both statements, the 
interpretation fails.
%
> iText (GTExclMark  phr text) = pure twoStatements <*> iPhr phr <*> iText text
> iText (GTFullStop  phr text) = pure twoStatements <*> iPhr phr <*> iText text
%
Any phrase followed by a question mark is interpreted as a yes/no question.
%
> iText (GTQuestMark phr GTEmpty) = pure toYNQuest <*> iPhr phr
%
An empty text is interpreted as true statement.
%
> iText GTEmpty = pure (Statement true)

%if unhandled
> iText text = unhandled "iText" text
%endif

\subsection{Phr: Phrases}

Phrases |Phr| are interpreted as inputs, that is, either statements or questions.
%
> iPhr :: GPhr -> I' Input
> iPhr (GPhrUtt GNoPConj utt GNoVoc) = iUtt utt 

%if unhandled
> iPhr phr = unhandled "iPhr" phr
%endif

\subsection{Utt: Utterances}

In the GF resource grammar, many categories can be used to build utterances |Utt|. We only 
consider declarative sentences and question sentences here.
%
> iUtt :: GUtt -> I' Input
%
Declarative sentence utterances are interpreted as statements.
%
> iUtt (GUttS s) = pure Statement <*> reset (iS s)
%
Question sentence utterances are interpreted as questions.
The type of question depends on the structure of the question sentence.
%
> iUtt (GUttQS qs) = iQS qs

%if unhandled
> iUtt utt = unhandled "iUtt" utt
%endif

\subsection{QS: Questions}

Question sentences |QS| are interpreted as one of the different types of questions.
We ignore the temporal features for now.
The polarity |Pol| argument is used to modify the inner |Prop| in
the question.
%
> iQS :: GQS -> I' Input
> iQS (GUseQCl temp pol qcl) = pure (\p c -> mapInput p c) <*> iPol pol <*> iQCl qcl


\subsection{QCl: Question clauses}

There are a number of question clause |QCl| constructs.
%
> iQCl :: GQCl -> I' Input
%
A declarative clause transformed into a yes/no question clause, 
e.g.~ ``does John walk''.
%
> iQCl (GQuestCl cl)          = pure YNQuest <*> reset (iCl cl)
%
An interrogative pronoun and a |Cl/NP|,
e.g.~``whom does John love''.
The clause is a scope island.
%
> iQCl (GQuestSlash ip clslash) = pure ($) <*> iIP ip <*> reset' (iClSlash clslash)
%
% Fool highlighting: $
%
An interrogative pronoun and a verb phrase,
e.g.~``who walks''. The verb phrase is a scope island.
%
> iQCl (GQuestVP ip vp)       = pure ($) <*> iIP ip <*> reset' (iVP vp)
%
% Fool highlighting: $
An existential question with an interrogative pronoun,
e.g.~``which houses are there''.
%
> iQCl (GExistIP ip)          = pure ($ (\x -> true)) <*> iIP ip
%
% Fool highlighting: $

%if unhandled
> iQCl qcl = unhandled "iQcl" qcl
%endif

% Fool highlighting: $

\subsection{IP: Interrogative Pronouns}

Interrogative pronouns take a one-place predicate
to a question of some kind.
%
> iIP :: GIP -> I' ((Ind -> Prop) -> Input)
%
An interrogative pronoun formed from an interrogative determiner 
and a common noun, e.g. ``which car''.
%
> iIP (GIdetCN idet cn) = iIDet idet <*> reset' (iCN cn)
%
Simple inanimate singular interrogative pronoun, e.g. ``what''.
%
> iIP GwhatSg_IP = pure (\u -> WhQuest u)
%
Simple animate singular interrogative pronoun, e.g. ``who''.
%
> iIP GwhoSg_IP  = pure (\u -> WhQuest u)

%if unhandled
> iIP ip = unhandled "iIP" ip
%endif

\subsection{IDet: Interrogative Determiners}

> iIDet :: GIDet -> I' ((Ind -> Prop) -> (Ind -> Prop) -> Input)
%
Interrogative quantifier with a number,
e.g. ``which five (men)''
%
> iIDet (GIdetQuant iquant num) = pure (\i u v -> YNQuest (i u v)) <*> reset'' (iIQuant iquant <*> iNum num)
%
Interrogative counting pronoun,
e.g. ``how many (men)''.
%
> iIDet Ghow8many_IDet = pure (\u v -> CountQuest (\x -> u x &&& v x))

%if unhandled
> iIDet idet = unhandled "iIDet" idet
%endif

Interrogative quantifier, e.g.~``which (man)''.
%
> iIQuant :: GIQuant -> I (((Ind -> Prop) -> (Ind -> Prop)) -> (Ind -> Prop) -> (Ind -> Prop) -> Prop)
> iIQuant Gwhich_IQuant = shift k (thereIs x (k (\num u v -> num (\y -> u y &&& v y) x)))

\subsection{S: Declarative Sentences}

Declarative sentences are interpreted as propositions.
%
> iS :: GS -> I Prop
%
Sentences combined with a contaunction.
%
> iS (GConjS conj ss) = pure foldr1 <*> iConj conj <*> iListS ss
%
A declarative clause with a polarity.
Ignores tense and anteriority for now.
%
> iS (GUseCl temp pol cl) = iPol pol <*> iCl cl
%
Adverbial phrase modifying a sentence.
This uses a special |iAdv| version.
%
> iS (GAdvS adv s) = iAdv_S adv <*> iS s

%if unhandled
> iS s = unhandled "iS" s
%endif

A list of sentences is interpreted as a list of propositions.
Each sentence is a scope island.
%
> iListS :: GListS -> I [Prop]
> iListS (GListS ss) = traverse (reset . iS) ss

\subsection{Cl: Declarative Clauses}

Declarative clauses are interpreted as propositions.
%
> iCl :: GCl -> I Prop
%
Verb phrase predication, e.g. ``John sleeps''.
%
> iCl (GPredVP np vp) = iNP np <*> iVP vp
%
Cleft constructions, e.g. ``it is John who sleeps''.
%
> iCl (GCleftNP np rs) = iNP np <*> iRS rs
%
Existential,
%(FIXME: what is this construction called)
e.g. ``there is a house''.
%
> iCl (GExistNP np) = iNP np <*> pure (\x -> true)

%if unhandled
> iCl cl = unhandled "iCl" cl
%endif

\subsection{Pol: Polarity}

Polarity is straightforwardly interpreted as a function over
propositions.
%
> iPol :: Applicative f => GPol -> f (Prop -> Prop)
> iPol GPPos = pure (\p -> p)
> iPol GPNeg = pure neg

\subsection{RS: Relative Sentences}

Relative clases are interpreted as predicates.
For now we ignore the tense and anteriority.
%
> iRS :: GRS -> I (Ind -> Prop)
> iRS (GUseRCl temp pol rcl) = pure (.) <*> iPol pol <*> iRCl rcl

\subsection{RCl: Relative Clauses}

Relative clauses are interpreted as predicates.
%
> iRCl :: GRCl -> I (Ind -> Prop)
%
Such-that construction, using a declarative clause as a relative
clause, e.g. ``such that a woman sleeps''.
This is mostly useful when there are anaphoric references
in the relative clause.
(Not that we handle anaphora yet).
%
> iRCl (GRelCl cl) = pure (\i x -> i) <*> iCl cl
%
A relative clause formed from a relative pronoun and a clause missing
a noun phrase (Cl/NP), e.g. ``which a woman loves''.
%
> iRCl (GRelSlash rp clslash) = iRP rp <*> iClSlash clslash
%
Forms a relative clause from a clause missing
a noun phrase, e.g. ``(woman) he loves''.
%
> iRCl (GEmptyRelSlash clslash) = iClSlash clslash
%
Relative clause with relative pronoun and verb phrase, e.g. ``that
sleeps''.
%
> iRCl (GRelVP rp vp) = iRP rp <*> iVP vp

%if unhandled
> iRCl rcl = unhandled "iRCl" rcl
%endif

\subsection{RP: Relative Pronouns}

Relative pronouns are interpreted as predicate modifiers.
%
> iRP :: GRP -> I ((Ind -> Prop) -> (Ind -> Prop))
%
A preposition and a noun phrase modifying a relative pronoun,
e.g. ``a part of which (is red)''.
%
> iRP (GFunRP prep np rp) = pure (\pi ni ri u x -> ni (\y -> (ri u) y &&& pi x y))
>                          <*> iPrep prep <*> iNP np <*> iRP rp
%
Relative pronoun, e.g.~``that'', ``which'', ``whose''.
%
> iRP GIdRP = pure (\u -> u)


\subsection{Slash: Clauses Missing NP}

Clause missing NP, Cl/NP.
%
> iClSlash :: GClSlash -> I (Ind -> Prop)
%
A Cl/NP formed from a VP/NP and an NP,
e.g.~``(which) a woman kills''.
%
> iClSlash (GSlashVP np vpslash) = 
>    pure (\ni vi x -> ni (vi (\u -> u x))) <*> iNP np <*> iVPSlash vpslash
%
|SlashPrep|: Prepositional Cl/NP, e.g. ``with (whom) john walks``.
This is problematic, since the |Cl| is interpreted as |Prop|,
but we need access to the subject of the clause, to give
that as one of the arguments to the preposition.
%
Adverb modifying a Cl/NP, 
e.g.~``(which) a woman kills in Paris''
%
> iClSlash (GAdvSlash clslash adv) = iAdv adv <*> iClSlash clslash

%if unhandled
> iClSlash clslash = unhandled "iClSlash" clslash
%endif

\subsection{Conj: Conjunctions}

Conjuctions are used in several places in the grammar, for example
on noun phrases, adjectival phrases and sentences.
They are binary operators on propositions.
%
> iConj :: GConj -> I (Prop -> Prop -> Prop)
%
``and'', interpreted as logical and.
%
> iConj Gand_Conj = pure (&&&)
%
``or'', interpreted as inclusive or.
%
> iConj Gor_Conj = pure (|||)
%
``both ... and ...'', intepreted as logical and.
%
> iConj Gboth7and_DConj = pure (&&&)
%
``either ... or ...'', interpreted as exclusive or.
%
> iConj Geither7or_DConj = pure (\p q -> (p &&& neg q) ||| (neg p &&& q))
%
> iConj Gif_then_Conj = pure (==>)

\subsection{VP: Verb Phrases}

Verb phrases are interpreted as one-place predicates.
%
> iVP :: GVP -> I (Ind -> Prop)
%
An adverbial phrase modifying a verb phrase,
e.g.~``sleeps in Paris''.
%
> iVP (GAdvVP vp adv) = iAdv adv <*> iVP vp
%
A pre-modifying adverbial phrase modifying a verb phrase,
e.g.~``always sleeps''.
%
> iVP (GAdVVP adv vp) = iAdV adv <*> iVP vp
%
A VP/NP and an NP object, e.g.~``kill a man''.
%
> iVP (GComplSlash vpslash np) = iVPSlash vpslash <*> iNP np
%
Passive use of transitive verb,
e.g.~``is killed''.
%
> iVP (GPassV2 v2) = pure (\vi x -> thereIs y (vi x y)) <*> iV2 v2
%
Reflexive use of transitive verb,
e.g.~``kills itself''.
%
> iVP (GReflVP vpslash) = pure (\i x -> i (\u -> u x) x) <*> iVPSlash vpslash
%
Copula with complement, e.g.~``is beautiful''.
%
> iVP (GUseComp comp) = iComp comp
%
Intransitive verb, e.g.~``sleeps''.
%
> iVP (GUseV v) = iV v
%
Progressive verb phrase, e.g.~``is sleeping''.
%
> iVP (GProgrVP vp) = iVP vp

%if unhandled
> iVP vp = unhandled "iVP" vp
%endif

\subsection{VPSlash: VP/NP}

Verb phrase missing an argument, VP/NP.
%
> iVPSlash :: GVPSlash -> I (((Ind -> Prop) -> Prop) -> (Ind -> Prop))
%
Transitive verb, e.g.~``kills (John)''.
%
> iVPSlash (GSlashV2a v2) = pure (\vi ni x -> ni (vi x)) <*> iV2 v2
%
Ditransitive verb with complement in second position.
%
``gives a dog (to Mary)''
%
> iVPSlash (GSlash2V3 v3 np) = pure (\vi ni1 ni2 x -> ni1 (\y -> ni2 (\z -> vi x y z))) <*> iV3 v3 <*> iNP np
%
Ditransitive verb with complement in third position.
%
``gives (a dog) to Mary''
%
> iVPSlash (GSlash3V3 v3 np) = pure (\vi ni1 ni2 x -> ni1 (\y -> ni2 (\z -> vi x z y))) <*> iV3 v3 <*> iNP np

%if unhandled
> iVPSlash vpslash = unhandled "iVPSlash" vpslash
%endif

\subsection{Comp: Complement of Copula}

Complement of copula, interpreted as a one-place predicate.
%
> iComp :: GComp -> I (Ind -> Prop)
%
Adjectival phrase complement, e.g. in ``(John is) very warm''.
%
> iComp (GCompAP ap) = iAP ap
%
Adverbial phrase complement, e.g. in ``(John is) in Paris''.
%
> iComp (GCompAdv adv) = iAdv adv <*> pure (\x -> true)
%
Noun phrase complement, e.g. in ``(John is) the owner of a car''.
The complement is a scope island, to get rid of the
reading of sentences such as ``every dog is an animal'' 
where they are all the same individual.
%
> iComp (GCompNP np) = reset' (pure (\ni x -> ni (\y -> x === y)) <*> iNP np)
%
Common noun complement, e.g. in ``(John is) a man''.
This is a special case of |CompNP|, which is much easier to
handle, especially in the case of plurals, 
e.g.~``all snakes are reptiles''.
%
> iComp (GCompCN cn) = iCN cn

\subsection{NP: Noun Phrases}

Noun phrases are type-raised individuals.
%
> iNP :: GNP -> I ((Ind -> Prop) -> Prop)
%
Noun phrase modified by an adverbial phrase,
e.g. ``Paris at midnight''.
%
> iNP (GAdvNP np adv) = pure (.) <*> iNP np <*> iAdv adv
%
Noun phrase modified by a relative sentence, 
e.g. ``Paris, which is a big city''.
%
> iNP (GRelNP np rs) = pure (\ni ri v -> ni (\x -> ri x &&& v x)) <*> iNP np <*> iRS rs
%
Noun phrase conjunction, e.g. ``John and a man''.
%
> iNP (GConjNP conj nps)   = pure (\ci ni u -> foldr1 ci [f u | f <- ni]) <*> iConj conj   <*> iListNP nps
%
Noun phrase formation from determiner and common noun,
e.g. ``every man''.
Note that there is no storage or anything like that here.
Instead the determiners use control operators to move
the quantifiers to the top level of the nearest enclosing scope
island. 
%
> iNP (GDetCN det cn) = iDet det <*> iCN cn
%
Mass expressions.
Note: this interpretation is dubious. We always
interpret mass expressions with existential quantifiers.
This is for example not correct for sentences such as 
``water is wet''.
%FIXME: universal as subject, existential in object position?
%FIXME: use shift?
%
> iNP (GMassNP cn) = pure (\u v -> thereIs x (u x &&& v x)) <*> iCN cn
%
Predeterminer use, e.g. ``only the men''. 
%FIXME: what should we do about predet + plural?
%
> iNP (GPredetNP predet np) = iPredet predet <*> iNP np
%
A noun phrase modified by a passive voice transitive verb, 
e.g. ``a woman killed''.
%
> iNP (GPPartNP np v2) = pure (\ni vi u -> ni (\x -> u x &&& thereIs y (vi x y))) <*> iNP np <*> iV2 v2
%
A proper name used as a noun phrase, e.g. ``John''.
%
> iNP (GUsePN pn) = pure (\i u -> u i) <*> iPN pn
%
A stand-alone determiner, e.g. ``these five (are good)''.
%
> iNP (GDetNP det) = iDet det <*> pure (\x -> true)
%
``everybody'', ``everything''
%
> iNP Geverybody_NP   = shift k (forAll x (k (\u -> u x)))
> iNP Geverything_NP  = shift k (forAll x (k (\u -> u x)))
%
``anybody'', ``anything''. Interpreted just like ``everybody'', ``everything''.
%
> iNP Ganybody_NP   = shift k (forAll x (k (\u -> u x)))
> iNP Ganything_NP  = shift k (forAll x (k (\u -> u x)))
%
``somebody'', ``something''
%
> iNP Gsomebody_NP   = shift k (thereIs x (k (\u -> u x)))
> iNP Gsomething_NP  = shift k (thereIs x (k (\u -> u x)))
%
``nobody'', ``nothing''
%
> iNP Gnobody_NP = shift k (neg (thereIs x (k (\u -> u x))))
> iNP Gnothing_NP = shift k (neg (thereIs x (k (\u -> u x))))

%if unhandled
> iNP np = unhandled "iNP" np
%endif

A list of noun phrases is interpreted as a list of type-raised 
propositions.
%
> iListNP :: GListNP -> I [(Ind -> Prop) -> Prop]
> iListNP (GListNP nps) = traverse iNP nps

\subsection{CN: Common Nouns}

Common nouns are one-place predicates.
%
> iCN :: GCN -> I (Ind -> Prop)
%
Common noun modified by an adjectival phrase, e.g. ``beautiful woman''.
%
> iCN (GAdjCN ap cn) = pure (\ai ci x -> ai x &&& ci x) <*> iAP ap <*> iCN cn
%
Common noun modified by an adverbial phrase, e.g. ``woman with a dog''.
%
> iCN (GAdvCN cn adv) = iAdv adv <*> iCN cn
%
Apposition of a common noun and a noun phrase,
e.g. ``king John''. This produces some unlikely-sounding 
constructions.
%
> iCN (GApposCN cn np) = pure (\ni ci x -> ni (x ===) &&& ci x) <*> iNP np <*> iCN cn
%
Complementation of a two-place noun, e.g. ``owner of a dog''.
%
> iCN (GComplN2 n2 np) = pure (\n2i npi x -> npi (\y -> n2i x y)) <*> iN2 n2 <*> iNP np
%
Common noun modified by a relative clause, e.g. ``man who sleeps''.
Relative clauses are scope islands.
%
> iCN (GRelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> reset' (iRS rs)
%
A noun used as a common noun, e.g. ``dog''.
%
> iCN (GUseN n) = iN n
%
A two-place noun used without a complement, e.g. ``owner''.
%
> iCN (GUseN2 n2) = pure (\ni x -> thereIs y (ni x y)) <*> iN2 n2
%
Compound common noun, e.g. ``Labour MP''.
The interpretation below is rather silly. For example,
``Labour MP'' is interpreted as $labour(X) \land mp(X)$, but the person is not the
party.
%
> iCN (GCompoundCN cn1 cn2) = pure (\ci1 ci2 x -> ci1 x &&& ci2 x) <*> iCN cn1 <*> iCN cn2

%if unhandled
> iCN cn = unhandled "iCN" cn
%endif

\subsection{Predet: Pre-determiners}

Predeterminers currently modify noun phrases, 
and as such are functions over noun phrase interpretations.
This is difficult to do anything reasonable with.
I think that the resource grammar API needs to be changed here.
%
> iPredet :: GPredet -> I (((Ind -> Prop) -> Prop) -> ((Ind -> Prop) -> Prop))
%
``only (John)''
Note that this doesn't work correctly with plurals, as in ``only men drink''.
%
> iPredet Gonly_Predet = pure (\np v -> np v &&& neg (thereIs x (v x &&& np (\y -> x =/= y))))
%
%``all (the men)''
%
%> iPredet Gall_Predet = pure (\np -> np)

> iPredet predet = unhandled "iPredet" predet

\subsection{Det: Determiners}

Determiners take two one-place predicates to a propositions.
%
> iDet :: GDet -> I ((Ind -> Prop) -> (Ind -> Prop) -> Prop)
%
A determiner, with a quantifier, a cardinal number
and an ordinal, e.g. ``these five best''.
%
> iDet (GDetQuantOrd quant num ord) = pure (\qi ni oi u v -> qi ni (oi u) v) <*> iQuant quant <*> iNum num <*> iOrd ord
%
A determiner with a quantifier with a cardinal number,
but no ordinal, e.g. ``these five''.
%
> iDet (GDetQuant quant num) = iQuant quant <*> iNum num
%
``every (man)''
%
> iDet Gevery_Det = shift k (forAll x (k (\u v -> u x ==> v x)))
``all (men)''.
The GF Resource Grammar API has ``all'' as a |Predet|, which also 
allows ``all the men''. However, this is overgenerating, and difficult to
interpret compositionally.
%
> iDet Gall_Det = shift k (forAll x (k (\u v -> u x ==> v x)))
%
``some (men)'
FIXME: Perhaps this should require there to be at least two.
%
> iDet GsomePl_Det = shift k (thereIs x (k (\u v -> u x &&& v x)))
%
``some (man)''. Same as |GDetQuant IndefArt NumSg|, ``a''.
%
> iDet GsomeSg_Det = shift k (thereIs x (k (\u v -> u x &&& v x)))
%
``neither (man)''. This is interpreted as meaning that there are exactly
two individuals with the first property, and they both lack 
the second property.
%
> iDet Gneither_Det = shift k (thereIs x ( thereIs y (forAll z (k (\u v -> u x &&& neg (v x) &&& u y &&& neg (v y) &&& x =/= y &&& (u z ==> (z === x ||| z === y)))))))
%
``both (men)''. This is interpreted as meaning that there are exactly
two individuals with the first property, and they both have 
the second property.
%
> iDet Gboth_Det = shift k (thereIs x ( thereIs y (forAll z (k (\u v -> u x &&& v x &&& u y &&& v y &&& x =/= y &&& (u z ==> (z === x ||| z === y)))))))
%
``either (man)'', interpreted as ``there are at least two, and at least one of them does it''.
%
> iDet Geither_Det = shift k (thereIs x ( thereIs y (k (\u v -> u x &&& u y &&& x =/= y &&& (v x ||| v y)))))
%
``many'', ``several'', ``few'', ``a few'', ``most''. 
FIXME: These are hard in FOL. We cheat by treating them as existentials.
%
> iDet Gmany_Det     = shift k (thereIs x (k (\u v -> u x &&& v x)))
> iDet Gseveral_Det  = shift k (thereIs x (k (\u v -> u x &&& v x)))
> iDet Ga8few_Det    = shift k (thereIs x (k (\u v -> u x &&& v x)))
> iDet Gfew_Det      = shift k (thereIs x (k (\u v -> u x &&& v x)))
> iDet Gmost_Det     = shift k (thereIs x (k (\u v -> u x &&& v x)))


%if unhandled
> iDet det = unhandled "iDet" det
%endif


\subsection{Quant: Quantifier}

%
> iQuant :: GQuant -> I (((Ind -> Prop) -> (Ind -> Prop)) -> (Ind -> Prop) -> (Ind -> Prop) -> Prop)
%
Indefinite article, ``a (man)'', or ``(men)''.
FIXME: this is not always correct in the plural.
For example, ``dogs are friendly'' should give a universial quantifier.
%
> iQuant GIndefArt = shift k (thereIs x (k (\num u v -> num (\y -> u y &&& v y) x)))
%
Definite article, ``the (man)'', or ``the (men)''.
%
> iQuant GDefArt = shift k (thereIs x (k (\num u v -> num (\y -> u y &&& v y) x &&& forAll z (u z ==> v z))))
%
Negated existential quantifier, ``no''.
%
> iQuant Gno_Quant = shift k (neg (thereIs x (k (\num u v -> num (\y -> u y &&& v y) x))))
%
Demonstrative, ``that (man)''.
FIXME: Should we also treat this as definite?
%
> iQuant Gthat_Quant = shift k (thereIs x (k (\num u v -> num (\y -> u y &&& v y) x &&& special "that" [x])))
%
Demonstrative,``this (man)''.
FIXME: Should we also treat this as definite?
%
> iQuant Gthis_Quant = shift k (thereIs x (k (\num u v -> num (\y -> u y &&& v y) x &&& special "this" [x])))
%
Genitive form of a noun phrase, e.g. ``john's (dog)''.
FIXME: Should this really allow more than one? Now ``john's dog'' allows john to have several dogs.
%
> iQuant (GGenNP np) = shift k (thereIs x (k (\n num u v -> num (\y -> u y &&& v y) x &&& n (\z -> special "genitive" [z,x])))) <*> iNP np

%if unhandled
> iQuant quant = unhandled "iQuant" quant
%endif


\subsection{Ord: Ordinal Number}

Ordinals and superlatives.
These are interpreted as modifying a one-place predicate.
%
> iOrd :: GOrd -> I ((Ind -> Prop) -> (Ind -> Prop))
%
Superlative adjective, e.g. ``largest''.
Interpreted in terms of the comparative. ``Largest'' is taken
to mean that there the individual is larger than all other individuals
identified by the restruction.
%
> iOrd (GOrdSuperl a) = pure (\ai u x -> u x &&& forAll y (u y ==> ai x y)) <*> iA_comparative a

%if unhandled
> iOrd ord = unhandled "iOrd" ord
%endif

\subsection{Card: Cardinal Number}

Cardinal numbers. Takes three arguments, the interpretation of an article 
or quantifier, the restriction and the sentence predicate.

> iCard :: GCard -> I ((Ind -> Prop) -> (Ind -> Prop))

Cardinal number with digits, e.g.~``5''.

> iCard (GNumDigits ds) = iInt (iDigits ds)

Cardinal number with words, e.g.~``five''.

> iCard (GNumNumeral num) = iInt (iNumeral num)

Cardinal modified by a numeral-modifying adjective, e.g. ``almost five''.

> iCard (GAdNum adn card) = iAdN adn <*> iCard card

%if unhandled
> iCard card = unhandled "iCard" card
%endif

Cardinal numbers. Takes three arguments, the interpretation of an article 
or quantifier, the restriction and the sentence predicate.

\subsection{AdN: Numeral-modifying adverb}

> iAdN :: GAdN -> I (((Ind -> Prop) -> (Ind -> Prop)) -> ((Ind -> Prop) -> (Ind -> Prop)))

% ``at least (five men)''.
%
%> iAdN Gat_least_AdN = pure (\n -> n)

> iAdN adn = unhandled "iAdN" adn

\subsection{Num: Number determining element}

> iNum :: GNum -> I ((Ind -> Prop) -> (Ind -> Prop))
%
Singular, e.g. ``dog''.
%
> iNum GNumSg = pure (\u x -> u x)
%
Plural, e.g.~``dogs''. 
This is currently interpreted as meaning ``at least two''.
This works for ``firemen are available'', ``criminals are ruining me''.
Other interpretations are needed for:
``no reptiles have fur'', ``dogs are friendly'',
``I like dogs''.
%
> iNum GNumPl = pure (\u x -> u x &&& thereIs y (u y &&& y =/= x))
%
A fixed number of distinct objects, given by a cardinal number.
%
> iNum (GNumCard card) = iCard card

%if unhandled
> iNum num = unhandled "iNum" num
%endif

\subsection{AP: Adjectival Phrases}

Adjectival phrases are interpreted as one-place predicates.
%
> iAP :: GAP -> I (Ind -> Prop)
%
Complementation of a two-place adjective, e.g. ``equivalent to a dog''.
%
> iAP (GComplA2 a2 np) = pure (\ai ni x -> ni (\y -> ai x y)) <*> iA2 a2 <*> iNP np
%
Adjectival phrase conjunction, e.g. ``big and red''.
%
> iAP (GConjAP conj aps)   = pure (\ci ai x -> foldr1 ci [f x | f <- ai]) <*> iConj conj <*> iListAP aps
%
Positive form of an adjective, e.g. ``big''.
%
> iAP (GPositA a) = iA a
%
Comparative form of an adjective, e.g. ``bigger''.
We introduce a new \textsf{more\_X} predicate for every adjective \textsf{X}.
%
> iAP (GComparA a np) = pure (\ai ni x -> ni (\y -> ai x y)) <*> iA_comparative a <*> iNP np
%
Reflexive use of a two-place adjective, e.g. ``equivalent to itself''.
%
> iAP (GReflA2 a2) = pure (\ia x -> ia x x) <*> iA2 a2
%
Existential use of a two-place adjective, e.g. ``(this dog is) equivalent''.
This is interpreted as have an implied ``(to something)''.
%
> iAP (GUseA2 a2) = pure (\ai x -> thereIs y (ai x y)) <*> iA2 a2
%
Adverb modifying an adjectival phrase, e.g. ``really big''.
FIXME: we are cheating by ignoring the adverb
%
> iAP (GAdAP ada ap) = iAP ap

%if unhandled
> iAP ap = unhandled "iAP" ap
%endif 

A list of adjectival phrases for use in adjectival phrase conjunction.
This is interpreted as a list of one-place predicates.
%
> iListAP :: GListAP -> I [Ind -> Prop]
> iListAP (GListAP aps) = traverse iAP aps


\subsection{Adv: Adverbial Phrases}

Adverbial phrases that can modify NP, CN, VP.
%
> iAdv :: GAdv -> I ((Ind -> Prop) -> (Ind -> Prop))
%
Adverbial phrase conjunction.
%
> iAdv (GConjAdv conj advs)   = pure (\ci ai u x -> foldr1 ci [f u x | f <- ai]) <*> iConj conj   <*> iListAdv advs
%
Prepositional phrase.
%
> iAdv (GPrepNP prep np) = pure (\pi ni u x -> u x &&& ni (\y -> pi x y)) <*> iPrep prep <*> iNP np
%
Subjunctive phrase.
%
> iAdv (GSubjS subj s) = pure (\sui si u x -> sui si (u x)) <*> iSubj subj <*> iS s
%
``here''
%
> iAdv Ghere_Adv = pure (\u x -> u x &&& Pred "here" [x])
%
``there''
%
> iAdv Gthere_Adv = pure (\u x -> u x &&& Pred "there" [x])

%if unhandled
> iAdv adv = unhandled "iAdv" adv
%endif

A list of adverbs for use in adverb conjunction.
This is interpreted as a list of one-place predicate modifiers.
%
> iListAdv :: GListAdv -> I [(Ind -> Prop) -> (Ind -> Prop)]
> iListAdv (GListAdv advs) = traverse iAdv advs

Interpretation of sentence-modifying adverbial phrases.
%
> iAdv_S :: GAdv -> I (Prop -> Prop)
%
Subjunctions with an sentence use as an adverb, e.g. ``(John walks) if (Mary runs)''.
%
> iAdv_S (GSubjS subj s) = iSubj subj <*> iS s

%if unhandled
> iAdv_S adv = unhandled "iAdv_S" adv
%endif

\subsection{AdV: Adverb directly attached to verb}

Interpreted as one-place predicate (verb phrase interpretation) modifier.
%
> iAdV :: GAdV -> I ((Ind -> Prop) -> (Ind -> Prop))

%if unhandled
> iAdV adv = unhandled "iAdV" adv
%endif

\subsection{Subj: Subjunctions}

Subjunctions combine two sentence interpretations (i.e~propositions).
%
> iSubj :: GSubj -> I (Prop -> Prop -> Prop)
%
``Although'' is interpreted as logical conjunction.
We ignore the implicature that the second proposition
is \emph{unexpectedly} true.
%
> iSubj Galthough_Subj = pure (&&&)
%
``Because'' is also interpreted as conjunction.
An alternative interpretation is implication, but that gives odd results.
For example, ``John runs because Mary runs'', would then mean that
it could be the case that Mary doesn't run.
%
> iSubj Gbecause_Subj = pure (&&&)
%
``If'' is plain implication.
%
> iSubj Gif_Subj = pure (==>)
%
``When'' is also interpreted as implication.
We ignore the reading where the events have to take place at the same time.
%
> iSubj Gwhen_Subj = pure (==>)

%if unhandled
> iSubj subj = unhandled "iSubj" subj
%endif


\subsection{Lexical Categories}

Simple nouns, e.g. ``dog'' are interpreted as one-place predicates.
%
> iN :: GN -> I (Ind -> Prop)
> iN (LexN n) = pure (\x -> Pred (symbol n) [x])

Two-place nouns, e.g.~``owner of (a bar)'', are interpreted
as two-place predicates.
%
> iN2 :: GN2 -> I (Ind -> Ind -> Prop)
> iN2 (GComplN3 n3 np) = pure (\n3i npi x y -> npi (\z -> n3i x z y)) <*> iN3 n3 <*> iNP np
> iN2 (GUse2N3 n3) = pure (\n3i x y -> thereIs z (n3i x z y)) <*> iN3 n3
> iN2 (GUse3N3 n3) = pure (\n3i x y -> thereIs z (n3i x y z)) <*> iN3 n3
> iN2 (LexN2 n2) = pure (\x y -> Pred (symbol n2) [x,y])

Three-place nouns, e.g. ``distance from (the hourse) to (the street)'',
are interpreted as three-place predicates.
%
> iN3 :: GN3 -> I (Ind -> Ind -> Ind -> Prop)
> iN3 (LexN3 n3) = pure (\x y z -> Pred (symbol n3) [x,y,z])

Proper names, e.g. ``John'', are interpreted as individuals.
%
> iPN :: GPN -> I Ind
> iPN (LexPN pn) = pure (Const (symbol pn))

Adjectives, e.g. ``big'', are one-place predicates.
%
> iA :: GA -> I (Ind -> Prop)
> iA (LexA a) = pure (\x -> Pred (symbol a) [x])

Comparative forms of adjectives are two-place predicates:
%
> iA_comparative :: GA -> I (Ind -> Ind -> Prop)
> iA_comparative (LexA a) = pure (\x y -> comparative_Pred (symbol a) x y)

Two-place adjectives, e.g. ``equivalent to (a dog)'', are interpreted as 
two-place predicates.
%
> iA2 :: GA2 -> I (Ind -> Ind -> Prop)
> iA2 (LexA2 a2) = pure (\x y -> Pred (symbol a2) [x,y])

Intransitive verbs, e.g. ``sleep'', are one-place predicates.
%
> iV :: GV -> I (Ind -> Prop)
> iV (LexV v) = pure (\x -> Pred (symbol v) [x])

Transitive verbs, e.g. ``kill'', are two-place predicates.
%
> iV2 :: GV2 -> I (Ind -> Ind -> Prop)
> iV2 (LexV2 v2) = pure (\x y -> Pred (symbol v2) [x,y])

Ditransitive verbs, e.g. ``give'', are three-place predicates.
%
> iV3 :: GV3 -> I (Ind -> Ind -> Ind -> Prop)
> iV3 (LexV3 v3) = pure (\x y z -> Pred (symbol v3) [x,y,z])

Prepositions, e.g. ``in'', are two-place predicates.
%
> iPrep :: GPrep -> I (Ind -> Ind -> Prop)
> iPrep (LexPrep prep) = pure (\x y -> Pred (symbol prep) [x,y])

\subsection{Digits: Cardinals and Ordinals in Digits}

Digits are interpreted as integers, and then handled by |iInt|.

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

\subsection{Int: Integers}

Integer interpretation. Used above for letter and digit numerals.

> iInt :: Int -> I ((Ind -> Prop) -> (Ind -> Prop))
> iInt 0 = pure (\u x -> neg (u x))
> iInt n = pure (\ni u x -> u x &&& ni u [x]) <*> iInt' (n-1)

> iInt' :: Int -> I ((Ind -> Prop) -> ([Ind] -> Prop))
> iInt' 0 = pure (\u xs -> forAll x (u x ==> oneOf x xs))
> iInt' n = pure (\ni u xs -> thereIs x (u x &&& neg (oneOf x xs) &&& ni u (x:xs))) <*> iInt' (n-1)

> oneOf :: Ind -> [Ind] -> Prop
> oneOf x xs = ors (map (x ===) xs)

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

> of_Pred :: Ind -> Ind -> Prop
> of_Pred x y = special "of" [x,y]

> comparative_Pred :: String -> Ind -> Ind -> Prop
> comparative_Pred a x y = special ("more_" ++ a) [x,y]

> special :: String -> [Ind] -> Prop
> special x = Pred ("special_" ++ x) 

--
-- * Utilities
--

> symbol :: String -> String
> symbol = map toLower . takeWhile (/='_')

> unhandled :: Gf a => String -> a -> b
> unhandled f x = throwUnhandledTree f (gf x)

> data UnhandledTree = UnhandledTree { unhandledFunction :: String, unhandledSubtree :: Tree }
>  deriving (Show,Typeable)

> throwUnhandledTree :: String -> Tree -> a
> throwUnhandledTree f t = throwDyn $ UnhandledTree f t

> tryUnhandledTree :: IO a -> IO (Either UnhandledTree a)
> tryUnhandledTree x = catchDyn (liftM Right x) (return . Left)


%endif
