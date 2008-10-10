% -*- Mode: LaTeX; coding:utf-8 -*-

%if not report

\documentclass[a4paper]{article}
\usepackage{natbib}

\let\cite=\citep
\let\shortcite=\citeyearpar

%include polycode.fmt

\title{Delimited Continuations, Applicative Functors and Natural Language Semantics}

\author{
  Bj\"{o}rn Bringert \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology and University of Gothenburg\\
  \texttt{bringert{\char64}chalmers.se}}

\begin{document}

\maketitle

\begin{abstract}

\end{abstract}

%endif

%{
%include semantics.fmt

\section{Introduction}

Montague~\shortcite{montague73:ptq} uses compositional rules that
map English expressions to lambda calculus terms over a higher-order
logic. 
There is a large body of work that extends this treatment to cover 
additional phenomena, such as scope ambiguities.
These efforts, such as Cooper storage~\cite{cooper83:quantification},
are often expressed as extensions to the
core lambda calculus.
We wish to stay within a (polymorphically) typed
lambda calculus.
Continuations have been proposed as an approach to deal
with a range of semantic 
phenomena~\cite{barker02:continuations-quantification}.
We show that it is possible to handle scope ambiguity and scope islands
in a polymorphically typed lambda calculus. The language that we use has
some syntactic extensions to lambda calculus, but these have straightforward
translations to the core calculus.

We use first-order logic as our target language, rather than the higher-order 
logic used by Montague~\shortcite{montague73:ptq}. 
This lets us use our semantics for practical applications with exisiting 
automated first-order reasoning tools.

It is a common approach to try to push as much as possible
of the semantics into the lexicon, while keeping the combination rules
as simple as possible. We have taken the opposite approach, where we try
to make the semantics of lexical items as simple as possible, 
since there are many more lexicon entries than syntactic constructs.
We also feel that  moving more to the semantics of syntactic constructs makes the semantics
easier to understand.

For example, is a lexicalized approach, the interpretation of adjectives may be of 
type |(Ind -> Prop) -> (Ind -> Prop)|, where |Ind -> Prop| is the interpretation type
of common nouns. This lets us use plain function application to implement 
adjectival modification of common nouns (e.g.~``tall man''). 
However, each adjective (e.g. |tall|) must 
then be interpreted as a function such as |\p -> \x -> tall(x) &&& p(x)|.
This makes it difficult to handle predicative use of adjectives 
(e.g.~``John is tall''). If the noun phrase ``John'' is interpreted as
|\p -> p(John)| (using \emph{Montague's trick}, also known as \emph{type raising}),
we need a way to turn |\p -> \x -> tall(x) &&& p(x)| into |\x -> tall(x)|.
This can be done by applying the adjective interpretation to |\x -> true|, before
applying the noun phrase interpretation to it. But now we have complicated 
the predication interpretation just because we wanted a simple modification interpretation.

Instead, we give each lexical category a straightforward interpretation type.
For example, adjectives are one-place predicates, |Ind -> Prop|, e.g.~|\x -> tall(x)|.
Now, the modification rule needs to use conjunction, but the predication rules becomes
simpler. In general, giving lexical items simple types seems to allow us to extend 
the semantics more easily, since we avoid pushing the interpretation of 
arbitrary syntactic constructs into the lexicon.

In this paper, we present the semantics for a progression of increasingly 
large fragments of the example GF abstract syntax shown in Figure~\ref{fig:Toy-gf}.

\section{Syntax}

In order to write Haskell functions over GF abstract syntax terms, we first
need a Haskell representation of such terms. The code below has been
automatically generated by GF from the abstract syntax module 
in Figure~\ref{fig:Toy-gf}.

FIXME: do we really need to show the generated data types here?
If so, how to include them automatically? And what about the G prefix?

\section{Semantics}

We interpret each abstract syntax term as a term in
$\lambda$-calculus over first-order logic with equality.
We use the customary connectives and quantifiers,
$n$-ary predicates, equality, inequality.

Formulas:

$\phi, \psi ::= $
$\phi \land \psi$,
$\phi \lor \psi$,
$\phi \Rightarrow \psi$,
$\lnot \phi$,
$\forall u \mathpunct{.} \phi$,
$\exists u \mathpunct{.} \phi$,
$x = y$,
$x \neq y$,
$\text{p}(x_1,\ldots,x_n)$,
$true$,
$false$

Expressions:

$x ::=$
$u$,
$\text{C}$

A term in our $\lambda$-calculus is a function application,
an abstraction (function value), a variable or a first-order logic formula.
FIXME: not true, they can be intermingled.

Lambda calculus:

$e ::=$
$e \ e'$,
$\lambda x \mathpunct{.} e$,
$x$,
$\phi$

Types:

$\tau ::=$
$\tau \rightarrow \tau'$,
$\mathsf{Ind}$,
$\mathsf{Prop}$


When we show formulas which are the result of semantic interpretation,
the will sometimes be simplified according to the rules of first-order logic.

\subsection{Fragment 1: Basics}

Our first language fragment only contains sentences made up of 
proper names, and transitive and intranstive verbs, 
along with the neccessary predication and complementation rules.
This lets use handle sentences such as ``John walks'' and ``John loves Mary'',
which are assigned the first-order logic formulas
$walk(John)$ and $love(John,Mary)$, respectively.

%if sem_toy_1_code || sem_toy_2_code || style /= newcode

%if style == newcode

> import Toy
> import FOL
> import TestToy
> import Prelude hiding (pred)

%endif

We first provide straightforward semantics for the lexical items.
Proper names are plain constants:
%
> iPN :: PN -> Ind
> iPN John  = Const "John"
> iPN Mary  = Const "Mary"
> iPN Bill  = Const "Bill"
%
Intransitive verbs are one-place predicates:
%
> iV :: V -> (Ind -> Prop)
> iV Walk = \x -> pred "walk" (x)
%
Transitive verbs are two-place predicates:
%
> iV2 :: V2 -> (Ind -> Ind -> Prop)
> iV2 Eat   = \x y -> pred "eat" (x,y)
> iV2 Love  = \x y -> pred "love" (x,y)

%endif

%if sem_toy_1_code || style /= newcode
%
Now for the semantics of the syntactic constructs.
Noun phrases, which are only proper names at this point,
are interpreted as individuals:
%
> iNP :: NP -> Ind
> iNP (UsePN pn) = iPN pn
%
Like intransitive verbs, verb phrases are interpreted as one-place predicates.
%
> iVP :: VP -> (Ind -> Prop)
%
For verb phrases formed from just an intransitive verb, nothing needs to be done:
%
> iVP (UseV v)         = iV v
%
In the case of transitive verb complementation, 
the interpretation of the object is given as the second 
argument to the two-place predicate which is the interpretation of the transitive verb:
%
> iVP (ComplV2 v2 np)  = \y -> (iV2 v2) y (iNP np)
%
Sentences are interpreted as propositions, and verb phrase predication is simply
the application of the one-place verb phrase predicate to the noun phrase individual.

> iS :: S -> Prop 
> iS (PredVP np vp) = (iVP vp) (iNP np)


%endif


\subsection{Fragment 2: Adding determiners}

%if sem_toy_2_code || style /= newcode

When we add determiners to our language fragment, we will need some way to handle
quantifiers, as we would for example like the sentence ``everyone walks'' to
have the interpretation |forAll x (pred "walk" (x))|.
Our previous type of NP interpretations, |Ind|, is insufficient
since we need to be able to introduce the universial quantifier
on the top-level of the formula.
Montague~\cite{montague73:ptq} solved this problem by changing the type 
of NP interpretations to |(Ind -> Prop) -> Prop|.

We first add interpretations for the remaining lexical categories.
Simple nouns are one-place predicates:
%
> iN :: N -> (Ind -> Prop)
> iN Man     = \x -> pred "man" (x)
> iN Woman   = \x -> pred "woman" (x)
> iN Burger  = \x -> pred "burger" (x)
%
Relational nouns are two-place predicates:
%
> iN2 :: N2 -> (Ind -> Ind -> Prop)
> iN2 Owner = \x y -> pred "owner" (x,y)
%
Noun phrases now have the higher type needed to include
quantified noun phrases:
%
> iNP :: NP -> ((Ind -> Prop) -> Prop)
> iNP Everyone    = \v -> forAll x (v x)
> iNP Someone     = \v -> thereIs x (v x)
%
Since we have changed the interpretation type of noun phrases,
we also need to change the rule for |UsePN|, which lifts proper names 
to noun phrases:
%
> iNP (UsePN pn)  = \v -> v (iPN pn)
%
We can also handle noun phrases consisting of a determiner and a common noun, 
as in ``every man walks'',
which we would like to interpret as $\forall x. man(x) \Rightarrow walk(x)$.
%
> iNP (DetCN det cn) = (iDet det) (iCN cn)
%
That requires determiners to be interpreted as two-place predicates
over one-place predicates. The first argument is the restriction
and the second is the FIXME: what is this called?
%
> iDet :: Det -> (Ind -> Prop) -> (Ind -> Prop) -> Prop
> iDet Every  = \u v -> forAll x (u x ==> v x)
> iDet A      = \u v -> thereIs x (u x &&& v x)
%
We also have relative sentences, which are interpreted as 
one-place predicates. E.g.~``who walks'', (|RelVP (UseV Walk)|) 
is interpreted as |\x -> walk(x)|.
%
> iRS :: RS -> (Ind -> Prop)
> iRS (RelVP vp) = iVP vp
%
Common nouns are one-place predicates.
%
> iCN :: CN -> (Ind -> Prop)
%
For simple nouns, there is nothing left to do:
%
> iCN (UseN n)         = iN n
%
Relational noun complementation is handled by letting the second argument
of the relational noun be filled by the complement.
%
> iCN (ComplN2 n2 np)  = \x -> (iNP np) ((iN2 n2) x)
%
or, equivalently, using the function composition operator |.|:
%
> iVP (ComplN2 n2 np) = iNP np . iN2 n2
%
Relative sentences modifying a common noun just take the logical
conjunction of the interpretations of the noun and the relative sentance.
%
> iCN (RelCN cn rs)    = \x -> (iCN cn) x &&& (iRS rs) x
%
Since we have changed the type of noun phrase interpretations,
we also need to change the rules which make use of noun phrases.
In the case of NP VP sentences, all we need to do is to change the 
order of application; we apply the noun phrase 
interpretation (|(Ind -> Prop) -> Prop|) to the verb phrase interpretation
(|Ind -> Prop|) to get the final |Prop|).
%
> iS :: S -> Prop 
> iS (PredVP np vp) = (iNP np) (iVP vp)
%
We also need to change the rule for transitive verb complementation since it
uses NP:
%
%if style == newcode
> iVP :: VP -> (Ind -> Prop)
> iVP (UseV v)         = iV v
%endif
%
> iVP (ComplV2 v2 np) = \x -> (iNP np) ((iV2 v2) x)
%
This could also be written using |.|, as with |ComplN2| above.

%endif

\subsection{Fragment 3: Quantifier scope ambiguity}

Consider a sentence such as ``every man loves a woman''.
The rules in the previous section would interpret this as
%
\begin{spec}
forAll x (pred "man" (x) ==> thereIs y (pred "woman" (y) &&& pred "love" (x,y))
\end{spec}
%
However, the sentence also has the alternative meaning
%
\begin{spec}
thereIs y (pred "woman" (y) &&& forAll x (pred "man" (x) ==> love(x,y)
\end{spec}
%
that is, that there is some woman whom every man loves.
To be able to generate both these readings, we need to allow the quantifiers from
a nested noun phrase to escape to the top level of the formula.
We also need a mechanism for allowing interpretation to be non-deterministic.

A number of approaches have been proposed to handle
\emph{quantifier scope ambiguity}, such as 
Cooper storage~\cite{cooper83:quantification}, and its improved version,
Keller storage~\cite{keller88:nester-cooper-storage}.
While it is possible to implement Keller storage in a typed
lambda calculus, the result is not very elegant. Cooper storage
seems difficult to implement in a typed way, because of the 
unsoundness that Keller pointed out.

Instead of storage, we can use \emph{continuations} to deal with 
quantifier scope.
Barker~\shortcite{barker02:continuations-quantification} shows 
that contiuations can be used to handle a range of semantic phenomena,
and that quantifier scope ambiguity can be handled by 
using a non-deterministic evaluation order in the continuized
semantics.

A \emph{continuation monad} can be used to hide the plumbing details 
of continuation passing style in natural language 
semantics~\cite{shan01:monads-natural-language}.
As Shan notes, one advantage of using a monadic style is that the monad
in question
can easily be replaced with a more elaborate one when we want to
account for additional details, 
without having to change all the interpretation rules.
However, it is in general not possible to make a continuation 
monad with non-deterministic 
evaluation order, since the\emph{bind} operation of a monad 
($\star$ in \cite{shan01:monads-natural-language}, |>>=| in Haskell) 
requires left-to-right evaluation.
But, fortunately for us, monads can be generalized to 
\emph{applicative functors}~\cite{mcbride07:applicative},
whose combining operator (|<*>|) is order-agnostic.
Thus, instead of a continuation monad for natural language semantics,
we propose a \emph{continuation applicative functor} with non-deterministic
evaluation order. In this section, we show that an applicative functor is sufficient for
the needs of our semantics, and that it can handle 
quantifier scope ambiguities. 

\subsubsection{Interpretation Functor}

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

%if style == newcode

> import Toy
> import FOL
> import TestToy
> import Prelude hiding (pred)

> test1 :: IO ()
> test1 = test (eval . iS) "a man who loves every woman eats a burger"

%endif

In the interest of readability, we will use set notation (e.g.~|[x,y,z]|) below.
This can be straightforwardly reaplced by the Church encoding of lists
to obtain a pure lambda calculus implementation.

A continuized computation is a set of functions that return a value given
its continuation (context). We will use |Cont o a| to mean
``a non-deterministic computation that returns a value of type |o|,
and that gives access to a continuation that accepts an argument of type |a|''.

> type Cont o a = [(a -> o) -> o]

The |pure| function lifts a pure value into the functor.
%
> pure :: a -> Cont o a
> pure a = [\c -> c a]
%
The |<*>| operator performs lifted function application.
In case the reader is concerned with the exponential behavior
of this implementation, we refer to section~\ref{sec:efficiency}.
%
> (<*>) :: Cont o (a -> b) -> Cont o a -> Cont o b
> xs <*> ys  =   [\c -> x (\f -> y (\a -> c (f a))) | x <- xs, y <- ys]
>            ++  [\c -> y (\a -> x (\f -> c (f a))) | x <- xs, y <- ys]


The |pure| and |<*>| functions make this an 
\emph{applicative functor}~\cite{mcbride07:applicative}.
Applicative functors generalize
\emph{monads}~\cite{wadler92:monads,moggi89:monads}.
Shan~\shortcite{shan01:monads-natural-language}
shows how a continuation monad can be used to handle quantification.
However, that treatment does not take quantifier scope ambiguity into account.

We also define an evaluation operator |eval|
(symbol borrowed from Shan~\cite{shan04:delimited-continuations}) which runs a 
computation and retrieves all possible results.
%
> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- x]

%{
%if style == newcode
%format shift = "shift"
%else 
%format shift = "\xi"
%endif
%
We finally add a control operator |shift| that gives access to the current continuation.
Its implementation turns out to be very simple:
%
> shift :: ((a -> o) -> o) -> Cont o a
> shift f = [f]
%
In the interest of readability, instead of |shift (\k -> e)|,
%}
we will write |shift k e|.


%endif

\subsubsection{Semantics}

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

In this example, the outer return type is always |Prop|,
so we use |I a| as a shorthand for |Cont Prop a|.

> type I a = Cont Prop a

We first lift the parts of our semantics that do not introduce any quantifiers
to use our new continuation functor.
Function application is replaced with lifted function application (|<*>|),
and |pure| is used to lift the combination functions where necessary.
%
> iS :: S -> I Prop 
> iS (PredVP np vp) = iNP np <*> iVP vp
>
> iVP :: VP -> I (Ind -> Prop)
> iVP (UseV v)         = iV v
%
As we noted above, the interpretations of transitive verb and relation noun complementation
can be written using function composition (|.|).
We now need to use lifted application of this operator.
Note that application of a pure (non-lifted) function |f| to 
a lifted argument |x| is written as |pure f <*> x|, and in the case
of a two-argument function, |pure f <*> x <*> y|.
%
> iVP (ComplV2 v2 np)  = pure (.) <*> iNP np <*> iV2 v2
%
Common noun interpretation is also the stright-forward lifting of 
the interpretation in the previouds section:
%
> iCN :: CN -> I (Ind -> Prop)
> iCN (UseN n)         = iN n
> iCN (ComplN2 n2 np)  = pure (.) <*> iNP np <*> iN2 n2
%
The interpretation of relational sentence modification looks a little hairy,
but it is really just the lifted version of |\x -> (iCN cn) x &&& (iRS rs) x|.
%
> iCN (RelCN cn rs)    = pure (\cn' rs' x -> cn' x &&& rs' x) <*> iCN cn <*> iRS rs
%
%if sem_toy_3_code || style /= newcode
%
Nothing needs to be done to the relative sentence interpretation, as it just returns
the already lifted result of interpreting a verb phrase.
%
> iRS :: RS -> I (Ind -> Prop)
> iRS (RelVP vp) = iVP vp

%endif 

Noun phrases and determiners introduce quantifiers.
Since we want to move quantifiers to the top-level,
these interpretations use the shift operator ($\xi$)
to obtain the current continuation and wrap the quantifier around it.
%
> iNP :: NP -> I ((Ind -> Prop) -> Prop)
> iNP Everyone        = shift k (forAll x (k (\v -> v x)))
> iNP Someone         = shift k (thereIs x (k (\v -> v x)))
> iNP (UsePN pn)      = pure (\x v -> v x) <*> iPN pn
> iNP (DetCN det cn)  = iDet det <*> iCN cn
%
%
> iDet :: Det -> I ((Ind -> Prop) -> (Ind -> Prop) -> Prop)
> iDet Every  = shift k (forAll x (k (\u v -> u x ==> v x)))
> iDet A      = shift k (thereIs x (k (\u v -> u x &&& v x)))

%endif

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

Finally, we need to lift the lexicon to use the interpretation functor.
This is very straightforward: we just use |pure| to lift 
each interpretation.
%
Simple nouns:
%
> iN :: N -> I (Ind -> Prop)
> iN Man     = pure (\x -> pred "man" (x))
> iN Woman   = pure (\x -> pred "woman" (x))
> iN Burger  = pure (\x -> pred "burger" (x))
%
Relational nouns:
%
> iN2 :: N2 -> I (Ind -> Ind -> Prop)
> iN2 Owner = pure (\x y -> pred "owner" (x,y))
%
Proper names:
%
> iPN :: PN -> I Ind
> iPN John  = pure (Const "John")
> iPN Mary  = pure (Const "Mary")
> iPN Bill  = pure (Const "Bill")
%
Intransitive verbs:
%
> iV :: V -> I (Ind -> Prop)
> iV Walk = pure (\x -> pred "walk" (x))
%
Transitive verbs:
%
> iV2 :: V2 -> I (Ind -> Ind -> Prop)
> iV2 Eat   = pure (\x y -> pred "eat" (x,y))
> iV2 Love  = pure (\x y -> pred "love" (x,y))

%endif

\subsection{Fragment 4: Scope islands}

A sentence such as ``a man who loves every woman eats a burger'' may at first
appear to have 6 readings since the 3 quantifiers can be ordered in $3! = 6$ ways.
However, we may not consider all of those readings to be sensible.

\begin{enumerate}
\item $\exists x. man(x) \land \forall y. woman(y) \land \exists z. burger(z) \land love(x,y) \land eat(x,z)$
\item * $\exists x. man(x) \land \exists z. burger(z) \land \forall y. woman(y) \land love(x,y) \land eat(x,z)$
\item * $\forall y. woman(y) \land \exists x. man(x) \land \exists z. burger(z) \land love(x,y) \land eat(x,z)$
\item * $\forall y. woman(y) \land \exists z. burger(z) \land \exists x. man(x) \land love(x,y) \land eat(x,z)$
\item * $\exists z. burger(z) \land \forall y. woman(y) \land \exists x. man(x) \land love(x,y) \land eat(x,z)$
\item $\exists z. burger(z) \land \exists x. man(x) \land \forall y. woman(y) \land love(x,y) \land eat(x,z)$
\end{enumerate}

Readings 2 and 4 are not generated by our interpretation,
in accordance with what Barker~\shortcite{barker01:integrity} calls the 
\emph{syntactic constituent integrity} scoping constraint.

Semantic theory (FIXME: reference!) has it that relative clauses 
are \emph{scope islands}, that is, no quantifier in the relative 
clause may take scope outside the relative clause.
This disallows readings 3,4,5.
But 3 and 5 are returned by our interpretation in Fragment 3.
It appears that we need a way to implement scope islands.

Shan~\cite{shan04:delimited-continuations} notes that 
\emph{delimited continuations} are useful for modelling several
natural language phenomena, and notes that 
Barker's~\shortcite{barker02:continuations-quantification}
treatment of scope islands implicitly uses $reset$ (|reset|).
Barker notes that this can only be done for categories
whose interpretation type is $t$ (|Prop| in our treatment).
However, as we show below, we can do it for
any type |tau_1 -> ... -> tau_n -> Prop| (though we have only needed
|Ind -> Prop| so far.


%if sem_toy_4_code || style /= newcode

The |reset| function for creating delimited continuation computations.

> reset :: Cont o o -> Cont p o
> reset x = [\c -> c y | y <- eval x]

Like |reset| but for single argument functions.

> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = [\c -> c y | y <- eval' x]

This uses a version of |eval| for single-argument functions.
This can be straight-forwardly be extended to any number of arguments,
but we have so far not found a needed for that.

> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- x]

The rule for relative sentences now uses |reset'| to make the
relative sentence a scope island.

> iRS :: RS -> I (Ind -> Prop)
> iRS (RelVP vp) = reset' (iVP vp)

%endif

With this addition, only readings 1 and 6 above are returned.

\section{Haskell implementation}

The semantics is implemented as a Haskell~\cite{haskell98} program.

\subsection{Efficiency}
\label{sec:efficiency}

The naive implementation above always uses both left-to-right and right-to-left 
evaluation whenever |<*>| is used.
This means that the number of formulas produced is roughly exponential
in the number of nodes in the abstract syntax tree. 
However, since only a few of the interpretation rules make use of 
$\xi$, there are many duplicates.
This blow-up can be avoided in most cases by chaning the implementation
of the continuation functor, as shown below.
Now, the number of formulas produced in exponential in the number of 
uses of $\xi$. For some sentences, this may still be a significant 
number, but these are now syntactically different readings.
Some of the may still be logically equivalent, for example if they only differ
in the order of adjacent universal quantifiers.

\subsubsection{A more efficient functor}

%if style /= newcode

%include Inter.lhs

%endif

\section{Related Work}

GF semantics in dependent type theory \cite{ranta04:semantics-type-theory}.
We share the idea of writing interpretation rules on the abstract 
syntax of a type theoretic grammar (GF in both cases).
However, Ranta uses a dependently typed lambda calculus where we
use a more pedestrian polymorpic lambda calculus.
Also, Ranta handles quantifier scope ambiguity by making the 
grammar ambiguous while keeping the interpretation rules
straightforward.

Compared to Barker, we have a more elgant way of writing
interpretation rules, since the applicative functor formulation
and control operators take care of the continuation plumbing.
Also, we implement non-deterministic evaluation order in a single place,
the definition of |<*>|, rather than having to add multiple interpretations
for each syntactic construct.

The idea of using a applicative functor for hiding the continuation
plumbing is very similar to Shan's use of monads for the same purpose,
but the restrictions of a monad does not allow the non-deterministic evaluation
order that we need.
Our 

Delimited continuations.

Blackburn and Bos.
We are using an explicit abstract syntax, which makes it
easier to change the surface language or the semantics independently.
In particular, this makes it pssoible for us to construct a multilingual
semantics.



%}



%if not report

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\end{document}

%endif