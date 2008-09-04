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
with a range of semantic phenomena~\cite{barker02:continuations-quantification}.


We use first-order logic as our target language, rather than the higher-order 
logic used by Montague~\shortcite{montague73:ptq}. 
This lets us use our semantics for practical applications with exisiting 
automated first-order reasoning tools.


Philosophy: don't push everything into the lexicon.
Lexical semantics should be as simple as possible, since there are more
lexicon entries than syntactic constructs.
Moving more to the semantics of syntactic constructs makes the semantics
easier to understand.
Example: adjectives as CN -> CN makes predicative use of adjectives
awkward.

Keeping semantics in the interpretation of syntax rather than in the lexicon
also lets us keep the lexicon largely unmodified when experimenting with 
different approaches to semantic interpretation.

We present the semantics for a progression of increasingly large fragments
of the example abstract syntax shown in Figure~\ref{fig:Toy-gf}.

\section{Syntax}

In order to write haskell functions over GF abstract syntax terms, we first
need a Haskell representation of such terms. The code below has been
automatically generated by GF from the abstract syntax module 
in Figure~\ref{fig:Toy-gf}.

FIXME: do we really need to show the generated data types here?
If so, how to include them automatically? And what about the G prefix?

\section{Semantics}

Lambda calculus over first-order logic.

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

We use the following syntax for first-order logic with equality:

Formulas:

$\phi, \psi ::= $
$\phi \land \psi$,
$\phi \lor \psi$,
$\phi \Rightarrow \psi$,
$\lnot \phi$,
$\forall x \mathpunct{.} \phi$,
$\exists x \mathpunct{.} \phi$,
$x = y$,
$x \neq y$,
$p(x_1,\ldots,x_n)$,

Expressions:

$x ::=$
$u$,
$\mathsf{C}$

When we show formulas which are the result of semantic interpretation,
the will sometimes be simplified according to the rules of first-order logic.

\subsection{Fragment 1: Basics}

The first fragment only contains transitive and intranstive verbs, and a single proper 
name, along with the neccessary predication and complementation rules.

%if sem_toy_1_code || sem_toy_2_code || style /= newcode

%if style == newcode

> import Toy
> import FOL
> import TestToy
> import Prelude hiding (pred)

%endif

We first provide straightforward semantics for the lexical items.

> iPN :: PN -> Ind
> iPN John  = Const "John"
> iPN Mary  = Const "Mary"
> iPN Bill  = Const "Bill"

> iV :: V -> (Ind -> Prop)
> iV Walk = \x -> pred "walk" (x)

> iV2 :: V2 -> (Ind -> Ind -> Prop)
> iV2 Eat   = \x y -> pred "eat" (x,y)
> iV2 Love  = \x y -> pred "love" (x,y)

> iN :: N -> (Ind -> Prop)
> iN Man     = \x -> pred "man" (x)
> iN Woman   = \x -> pred "woman" (x)
> iN Burger  = \x -> pred "burger" (x)

> iN2 :: N2 -> (Ind -> Ind -> Prop)
> iN2 Owner = \x y -> pred "owner" (x,y)

%endif

%if sem_toy_1_code || style /= newcode

> iS :: S -> Prop 
> iS (PredVP np vp) = (iVP vp) (iNP np)

> iNP :: NP -> Ind
> iNP (UsePN pn) = iPN pn

> iVP :: VP -> (Ind -> Prop)
> iVP (UseV v)         = iV v
> iVP (ComplV2 v2 np)  = (iV2 v2) (iNP np)

%endif

This lets use handle the sentences ``John walks'' and ``John loves John'',
which are assigned the first-order logic formulas
$walk(John)$ and $love(John,John)$, respectively.


\subsection{Fragment 2: Adding determiners}

%if sem_toy_2_code || style /= newcode

When we add determiners to our language fragment, we will need some way handling
quantifiers, as we would for example like the sentence ``everyone walks'' to
have the interpretation $forall x. walk(x)$.
Our previous type of NP interpretations, |Ind|, is insufficient
since we need to be able to introduce the universial quantifier.
Montague~\cite{montague73:ptq} solved this problem by changing the type 
of NP interpretations to |(Ind -> Prop) -> Prop|.

> iNP :: NP -> ((Ind -> Prop) -> Prop)
> iNP Everyone    = \v -> forAll x (v x)
> iNP Someone     = \v -> thereIs x (v x)

We also need to change the rule for |UsePN|, which lifts proper names 
to noun phrases:

> iNP (UsePN pn)  = \v -> v (iPN pn)

We can now also handle noun phrases consisting of a determiner and a common noun, 
as in ``every man walks'',
which we would like to interpret as $\forall x. man(x) \Rightarrow walk(x)$.

> iNP (DetCN det cn) = (iDet det) (iCN cn)

> iDet :: Det -> (Ind -> Prop) -> (Ind -> Prop) -> Prop
> iDet Every  = \u v -> forAll x (u x ==> v x)
> iDet A      = \u v -> thereIs x (u x &&& v x)

> iCN :: CN -> (Ind -> Prop)
> iCN (UseN n)         = iN n
> iCN (ComplN2 n2 np)  = \x -> (iNP np) ((iN2 n2) x)
> iCN (RelCN cn rs)    = \x -> (iCN cn) x &&& (iRS rs) x

> iRS :: RS -> (Ind -> Prop)
> iRS (RelVP vp) = iVP vp

Since we have changed the type of noun phrase interpretations,
we also need to change the rules which make use of noun phrases.
In the case of NP VP sentences, all we need to do is to change the 
order of application; all we do is to apply the noun phrase 
interpretation (|(Ind -> Prop) -> Prop|) to the verb phrase interpretation
(|Ind -> Prop|) to get the final |Prop|).
%
> iS :: S -> Prop 
> iS (PredVP np vp) = (iNP np) (iVP vp)

We also need to change the rule for transitive verb complementation:
%
> iVP (ComplV2 v2 np) = \x -> (iNP np) ((iV2 v2) x)
%
or, equivalently,
%
> iVP (ComplV2 v2 np) = (iNP np) . (iV2 v2)

%endif

\subsection{Fragment 3: Quantifier scope ambiguity}

Now, consider a sentence such as ``every man loves a woman''.
The rules in the previous section would interpret this as
$forall x. man(x) \Rightarrow exist y. woman(y) \land love(x,y)$.
However, the sentence also has the alternative meaning
$exist y. woman(y) \land forall x. man(x) \Rightarrow love(x,y)$,
that is, that there is some woman whom every man loves.
To be able to do this, we need to allow the quantifiers from
a nested noun phrase to escape to the top level of the formula.

A number of approaches have been proposed to handle such
\emph{quantifier scope ambiguity}, such as 
Cooper storage~\cite{FIXME}, and its improved successor,
Keller storage~\cite{keller88:nester-cooper-storage}.
While it is possible to implement Keller storage in a typed
lambda calculus, the result is not very elegant. Cooper storage
seems difficult to implement in a typed way, because of the 
unsoundness that Keller pointed out.

Instead of storage, we can use continuations to deal with 
quantifier scope.
Barker~\cite{barker02:continuations-quantification} notes
that Montague's trick is equivalent to continuation 
passin, and that quantifier scope ambiguity can be handled by
using a non-deterministic evaluation order.

A \emph{continuation monad} can be used to hide the plumbing details 
of continuation passing style in natural language 
semantics~\cite{shan01:monads-natural-language}.
As Shan notes, one advantage of using a monadic style is that the monad
in question
can easily be replaced with a more elaborate one when we want to
account for additional details, changing most of the interpretation rules,
as we do in the next section.
However, it is in general not possible to make a continuation 
monad with non-deterministic 
evaluation order, since the bind operation of a monad requires left-to-right
evaluation.
But, fortunately for us, monads can be generalized to 
\emph{applicative functors}~\cite{mcbride07:applicative},
whose combining operator is order-agnostic.
Thus, instead of a continuation monad for natural language semantics,
we propose a \emph{continuation applicative functor} with non-deterministic
evaluation order. In this section, we show that an applicative functor is sufficient for
the needs of a compositional natural language semantics, and that it can handle 
quantifier scope ambiguities. 

FIXME: first show simpler version which generates redundant interpretations?

\subsubsection{Interpretation Functor}

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

%if style == newcode

> import Toy
> import FOL
> import TestToy
> import Prelude hiding (pred)

> test1 = test (eval . iS) "a man who loves every woman eats a burger"

%endif


This is a representation of a non-deterministic continuation functor.

> type Cont o a = [(a -> o) -> o]

The |pure| function lifts a pure value into the functor.

> pure :: a -> Cont o a
> pure a = [\c -> c a]

The |<*>| operator performs lifted function application.
In case the reader is concerned with the exponential behavior
of this operator, we refer to section~\ref{sec:efficiency}.

> (<*>) :: Cont o (a -> b) -> Cont o a -> Cont o b
> xs <*> ys = concat [[  \c -> x (\f -> y (\a -> c (f a))),
>                        \c -> y (\a -> x (\f -> c (f a)))]
>                        | x <- xs, y <- ys]

Runs a compuation and retrives all posible results.

> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- x]

A computation that looks at the continuation.
%{
%if style == newcode
%format shift = "shift"
%else 
%format shift = "\xi"
%endif

> shift :: ((a -> o) -> o) -> Cont o a
> shift f = [f]

%}


%endif

\subsubsection{Semantics}

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

In this example, the outer return type is always |Prop|,
so we use |I a| as a shorthand for |Cont Prop a|.

> type I a = Cont Prop a

We lift the entire semantics to our new continuation functor.

> iS :: S -> I Prop 
> iS (PredVP np vp) = iNP np <*> iVP vp

$shift$ ($\xi$) is used here

> iNP :: NP -> I ((Ind -> Prop) -> Prop)
> iNP Everyone        = shift k (forAll x (k (\v -> v x)))
> iNP Someone         = shift k (thereIs x (k (\v -> v x)))
> iNP (UsePN pn)      = pure (\x v -> v x) <*> iPN pn
> iNP (DetCN det cn)  = iDet det <*> iCN cn

$shift$ is used here

> iDet :: Det -> I ((Ind -> Prop) -> (Ind -> Prop) -> Prop)
> iDet Every  = shift k (forAll x (k (\u v -> u x ==> v x)))
> iDet A      = shift k (thereIs x (k (\u v -> u x &&& v x)))

> iVP :: VP -> I (Ind -> Prop)
> iVP (UseV v)         = iV v
> iVP (ComplV2 v2 np)  = pure (.) <*> iNP np <*> iV2 v2

> iCN :: CN -> I (Ind -> Prop)
> iCN (UseN n)         = iN n
> iCN (ComplN2 n2 np)  = pure (.) <*> iNP np <*> iN2 n2
> iCN (RelCN cn rs)    = pure (\cn' rs' x -> cn' x &&& rs' x) <*> iCN cn <*> iRS rs

%endif

%if sem_toy_3_code || style /= newcode

> iRS :: RS -> I (Ind -> Prop)
> iRS (RelVP vp) = iVP vp

%endif 

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

Finally, we need to lift the lexicon to use the interpretation functor.
This is not strictly necessary, since we could instead wrap each call to
interpretation functions for lexical categories in |pure|.

> iN :: N -> I (Ind -> Prop)
> iN Man     = pure (\x -> pred "man" (x))
> iN Woman   = pure (\x -> pred "woman" (x))
> iN Burger  = pure (\x -> pred "burger" (x))

> iN2 :: N2 -> I (Ind -> Ind -> Prop)
> iN2 Owner = pure (\x y -> pred "owner" (x,y))

> iPN :: PN -> I Ind
> iPN John  = pure (Const "John")
> iPN Mary  = pure (Const "Mary")
> iPN Bill  = pure (Const "Bill")

> iV :: V -> I (Ind -> Prop)
> iV Walk = pure (\x -> pred "walk" (x))

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
any type |\tau_1 -> ... -> \tau_n -> Prop| (though we have only needed
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


%}



%if not report

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\end{document}

%endif