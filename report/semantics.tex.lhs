% -*- Mode: LaTeX; coding:utf-8 -*-

%if not report

\documentclass[a4paper]{article}
\usepackage{natbib}
\usepackage{verbatim}
\usepackage{xspace}

\let\cite=\citep
\let\shortcite=\citeyearpar
\newcommand{\lcalc}{$\lambda$-calculus\xspace}

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
We present a continuation-based approach to writing compositional semantics
for type-theoretic grammars which accounts for quantifier scope ambiguity
and scope islands. 
\end{abstract}

%endif

\section{Introduction}

Montague~\shortcite{montague73:ptq} uses compositional rules that
map English expressions to an intentional logic which combines
higher-order logic with \lcalc.
There is a large body of work that extends this treatment to cover 
additional phenomena, such as scope ambiguities.
These efforts, such as Cooper storage~\cite{cooper83:quantification},
are often expressed as extensions to the
core \lcalc.
We wish to stay within a (polymorphically) typed \lcalc.
Continuations have been proposed as an approach to deal
with a range of semantic 
phenomena~\cite{barker02:continuations-quantification}.
We show that it is possible to handle scope ambiguity and scope islands
in a polymorphically typed \lcalc. The language that we use has
some syntactic extensions to \lcalc, but these have straightforward
translations to the core calculus.

We use first-order logic as our target language, rather than the higher-order 
logic used by Montague~\shortcite{montague73:ptq}. 
This lets us use our semantics for practical applications with existing 
automated first-order reasoning tools.

%if not report
In Section \ref{sec:syntax} we present a fragment of English described using a 
Grammatical Framework (GF)~\cite{ranta04:gf} grammar.
%endif
Section \ref{sec:semantics} constitutes the bulk of this paper.
It presents a sequence of progressively refined
semantics for fragments of English.
We start by introducing the most basic machinery in
Section \ref{sec:frag-basics}, which handles 
a small language fragment with only proper names and
transitive and intransitive verbs.
In this fragment, all constructs have straightforward 
interpretations.
In Section \ref{sec:frag-det}, we add determiners,
along with simple nouns, relational nouns and relative clauses.
The problem of handling quantifiers in noun phrases is dealt with
using Montague's type raising trick.
In Section \ref{sec:frag-amb}, we tackle quantifier scope
ambiguity, by lifting our entire interpretation to
a dynamic logic, in this case an applicative functor
for continuation computations.
By using a non-deterministic evaluation order,
we can generate all possible quantifier scopings.
In Section \ref{sec:frag-island} we add a construct
for delimited continuations, and use it to handle scope 
islands.
Section \ref{sec:implementation} describes how this approach
can be implemented efficiently in the Haskell programming language.
Section \ref{sec:related-work} gives a brief overview of some related work.

%if not report && style /= newcode

%{
%include gf.fmt

\section{Syntax}
\label{sec:syntax}

Grammatical Framework (GF)~\cite{ranta04:gf} is a type-theoretical 
grammar formalism. 
In order to introduce GF, and to give us an example syntax to work with 
in the following section, we present an example grammar for a fragment
of English. The abstract syntax shown in Figure~\ref{fig:Toy-gf}
defines categories (|cat|) and functions (|fun|).
An example of an abstract syntax tree in this grammar is
%
\begin{spec}
|PredVP (DetCN Every (UseN Woman)) (UseV Walk)|.
\end{spec}
%
The concrete syntax shown in Figure~\ref{fig:ToyEng-gf} defines
the linearization type (|lincat|) of each abstract syntax category,
and the linearization (|lin|) of each abstract syntax function.
In this concrete syntax, the abstract syntax tree above is linearized
to:
%
\begin{spec}
{ s = ``every woman walks'' }
\end{spec}

In this simple example, no inflection, agreement or other
complex morphosyntactic features are needed to implement the English
concrete syntax. However, GF does allow more sophisticated linearization rules,
with records, finite functions and algebraic data types, which can be used to 
implement more complex grammars without changing the abstract syntax.
GF can be used to create multilingual grammars by associating multiple
concrete syntaxes with a single abstract syntax.

\begin{figure}
%include examples/toy/Toy.gf.lhs
\caption{\texttt{Toy.gf}: Abstract syntax for a small language fragment.}
\label{fig:Toy-gf}
\end{figure}

\begin{figure}
%include examples/toy/ToyEng.gf.lhs
\caption{\texttt{ToyEng.gf}: Concrete syntax for a small fragment of English.}
\label{fig:ToyEng-gf}
\end{figure}

%}

%endif

%{
%include semantics.fmt

\section{Semantics}
\label{sec:semantics}

Since we define the syntax with a GF grammar, we can write our interpretation
functions over GF abstract syntax trees, which abstract away from details
such as word order and agreement. 
We interpret each abstract syntax term as a term in a combination of 
\lcalc and first-order logic with equality.
We use the customary connectives and quantifiers,
$n$-ary predicates, equality, inequality.
%
\begin{comment}

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

A term in our \lcalc is a function application,
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

\end{comment}
%
When we show formulas which are the result of semantic interpretation,
the will sometimes be simplified according to the rules of first-order logic.

\subsection{Fragment 1: Basics}
\label{sec:frag-basics}

Our first language fragment only contains sentences made up of 
proper names, and transitive or intransitive verbs.
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

For each category |C| in the abstract syntax, there is a function
|iC :: C -> C*|, where |C*| is the interpretation type of |C|.

We first provide a straightforward semantics for the lexical items.
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
Transitive verbs are two-place predicates. We take the subject
as the first function argument, and the object as the second,
which is the opposite to what Montague does. 
In addition to being more intuitive (in the opinion of the author), 
this order lets us use function composition in the complementation
rules below.
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
argument to the two-place predicate which is the interpretation of the 
transitive verb (note that in our notation, function application
binds harder than $\lambda$-abstraction):
%
> iVP (ComplV2 v2 np)  = \y -> (iV2 v2) y (iNP np)
%
Sentences are interpreted as propositions, and verb phrase predication is simply
the application of the one-place verb phrase predicate to the noun phrase individual.

> iS :: S -> Prop 
> iS (PredVP np vp) = (iVP vp) (iNP np)


%endif


\subsection{Fragment 2: Adding determiners}
\label{sec:frag-det}

%if sem_toy_2_code || style /= newcode

When we add determiners to our language fragment, we will need some way to handle
quantifiers, as we would for example like the sentence ``everyone walks'' to
have the interpretation |forAll x (pred "walk" (x))|.
Our previous type of NP interpretations, |Ind|, is insufficient
since we need to be able to introduce the universal quantifier
on the top-level of the formula.
Montague~\shortcite{montague73:ptq} solved this problem by changing the type 
of NP interpretations to |(Ind -> Prop) -> Prop|, and we will do the same.

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
We can now handle noun phrases consisting of a determiner and a common noun, 
as in ``every man walks'',
which we would like to interpret as |forAll x (man(x) ==> walk(x))|.
%
> iNP (DetCN det cn) = (iDet det) (iCN cn)
%
That requires determiners to be interpreted as two-place predicates
over one-place predicates. 
%The first argument is the restriction
%and the second is the FIXME: what is this called?
%
> iDet :: Det -> (Ind -> Prop) -> (Ind -> Prop) -> Prop
> iDet Every  = \u v -> forAll x (u x ==> v x)
> iDet A      = \u v -> thereIs x (u x &&& v x)
%
We also have relative sentences, which are interpreted as 
one-place predicates. E.g.~``who walks'', |RelVP (UseV Walk)|
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
of the relational noun be filled by the complement (this is done in a roundabout
way because of the Montague trick).
%
> iCN (ComplN2 n2 np)  = \x -> (iNP np) ((iN2 n2) x)
%
or, equivalently, using the function composition operator |.|
(function application binds harder than composition):
%
> iCN (ComplN2 n2 np) = iNP np . iN2 n2
%
A relative sentence that modifies a common noun is interpreted as
the conjunction of the propositions resulting from the 
two constituents.
%
> iCN (RelCN cn rs)    = \x -> (iCN cn) x &&& (iRS rs) x
%
Since we have changed the type of noun phrase interpretations,
we also need to change the rules which make use of noun phrases.
In the case of NP VP sentences, all we need to do is to change the 
order of application. We apply the noun phrase 
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
\label{sec:frag-amb}

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
\lcalc, the result is not very elegant. Cooper storage
seems difficult to implement in a typed way, because of the 
unsoundness that Keller pointed out.

Instead of storage, we can use \emph{continuations} to deal with 
quantifier scope.
Barker~\shortcite{barker02:continuations-quantification} shows 
that continuations can be used to handle a range of semantic phenomena,
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
evaluation order, since the \emph{bind} operation of a monad 
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
This can be straightforwardly replaced by the Church encoding of lists
to obtain a pure \lcalc implementation.

A non-deterministic continuized computation is a set of functions that return a value given
its continuation (context). We will use |Cont o a| to mean
``a non-deterministic computation that returns a value of type |o|,
and that gives access to a continuation that accepts an argument of type |a|''.

> type Cont o a = [(a -> o) -> o]

The |raise| function lifts a pure value into the functor.
%
> raise :: a -> Cont o a
> raise a = [\c -> c a]
%
The |<*>| operator performs lifted function application
with both evaluation orders.
In case the reader is concerned with the exponential behavior
of this implementation, we refer to section~\ref{sec:efficiency}.
%
> (<*>) :: Cont o (a -> b) -> Cont o a -> Cont o b
> xs <*> ys  =   [\c -> x (\f -> y (\a -> c (f a))) | x <- xs, y <- ys]
>            ++  [\c -> y (\a -> x (\f -> c (f a))) | x <- xs, y <- ys]


The |raise| (also called |pure|) and |<*>| functions make this an 
\emph{applicative functor}~\cite{mcbride07:applicative}.
Applicative functors generalize
\emph{monads}~\cite{wadler92:monads,moggi89:monads}.
Shan~\shortcite{shan01:monads-natural-language}
shows how a continuation monad can be used to handle quantification.
However, that treatment does not take quantifier scope ambiguity into account.

We also define an evaluation operator |eval|
(symbol borrowed from Shan~\shortcite{shan04:delimited-continuations}) which runs a 
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
and |raise| is used to lift the combination functions where necessary.
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
a lifted argument |x| is written as |raise f <*> x|, and in the case
of a two-argument function, |raise f <*> x <*> y|.
As in Haskell, we use |(.)| as a shorthand for |(\f -> \g -> f . g)|.
%
> iVP (ComplV2 v2 np)  = raise (.) <*> iNP np <*> iV2 v2
%
Common noun interpretation is also the straightforward lifting of 
the interpretation in the previous section:
%
> iCN :: CN -> I (Ind -> Prop)
> iCN (UseN n)         = iN n
> iCN (ComplN2 n2 np)  = raise (.) <*> iNP np <*> iN2 n2
%
The interpretation of relational sentence modification looks a little hairy,
but it is really just the lifted version of |\x -> (iCN cn) x &&& (iRS rs) x|.
%
> iCN (RelCN cn rs)    = raise (\cn' rs' x -> cn' x &&& rs' x) <*> iCN cn <*> iRS rs
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
|UsePN| and |DetCN| are simply lifted.
%
> iNP :: NP -> I ((Ind -> Prop) -> Prop)
> iNP Everyone        = shift k (forAll x (k (\v -> v x)))
> iNP Someone         = shift k (thereIs x (k (\v -> v x)))
> iNP (UsePN pn)      = raise (\x v -> v x) <*> iPN pn
> iNP (DetCN det cn)  = iDet det <*> iCN cn
%
%
> iDet :: Det -> I ((Ind -> Prop) -> (Ind -> Prop) -> Prop)
> iDet Every  = shift k (forAll x (k (\u v -> u x ==> v x)))
> iDet A      = shift k (thereIs x (k (\u v -> u x &&& v x)))

%endif

%if sem_toy_3_code || sem_toy_4_code || style /= newcode

Finally, we need to lift the lexicon to use the interpretation functor.
This is very straightforward: we just use |raise| to lift 
each interpretation.
%
Simple nouns:
%
> iN :: N -> I (Ind -> Prop)
> iN Man     = raise (\x -> pred "man" (x))
> iN Woman   = raise (\x -> pred "woman" (x))
> iN Burger  = raise (\x -> pred "burger" (x))
%
Relational nouns:
%
> iN2 :: N2 -> I (Ind -> Ind -> Prop)
> iN2 Owner = raise (\x y -> pred "owner" (x,y))
%
Proper names:
%
> iPN :: PN -> I Ind
> iPN John  = raise (Const "John")
> iPN Mary  = raise (Const "Mary")
> iPN Bill  = raise (Const "Bill")
%
Intransitive verbs:
%
> iV :: V -> I (Ind -> Prop)
> iV Walk = raise (\x -> pred "walk" (x))
%
Transitive verbs:
%
> iV2 :: V2 -> I (Ind -> Ind -> Prop)
> iV2 Eat   = raise (\x y -> pred "eat" (x,y))
> iV2 Love  = raise (\x y -> pred "love" (x,y))

%endif

\subsection{Fragment 4: Scope islands}
\label{sec:frag-island}

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

Semantic theory 
%(FIXME: reference!) 
has it that relative clauses 
are \emph{scope islands}, that is, no quantifier in the relative 
clause may take scope outside the relative clause.
This disallows readings 3, 4, and 5.
But 3 and 5 are returned by our interpretation in Fragment 3.
It appears that we need a way to implement scope islands.

Shan~\shortcite{shan04:delimited-continuations} notes that 
\emph{delimited continuations} are useful for modeling several
natural language phenomena, and notes that 
Barker's~\shortcite{barker02:continuations-quantification}
treatment of scope islands implicitly uses $reset$ (|reset|).
Barker notes that this can only be done for categories
whose interpretation type is $t$ (|Prop| in our treatment).
However, as we show below, we can do it for
any type |tau_1 -> ... -> tau_n -> Prop| (though we have only seen a need
for |Ind -> Prop| so far).


%if sem_toy_4_code || style /= newcode

The |reset| function for creates a delimited continuation computation:
%
> reset :: Cont o o -> Cont p o
> reset x = [\c -> c y | y <- eval x]
%
We overload |reset| to also work on single argument functions.
We use overloading here to simplify the notation. To stay within
polymorphically typed \lcalc, we could simply use 
two different names instead.
%
> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = [\c -> c y | y <- eval' x]
%
This second version of |reset| uses a version of |eval| for single-argument functions:
%
> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- x]

The rule for relative sentences now uses |reset'| to make the
relative sentence a scope island.
%
> iRS :: RS -> I (Ind -> Prop)
> iRS (RelVP vp) = reset' (iVP vp)
%
%endif
%
With this addition, only readings 1 and 6 above are returned.

\section{Haskell implementation}
\label{sec:implementation}

The semantics is implemented as a Haskell~\cite{haskell98} program.
In fact, the rules that we have shown above are thinly camouflaged
Haskell code. A Haskell program can be automatically extracted 
from the source code for this paper. 

\subsection{Efficiency}
\label{sec:efficiency}

The naive implementation above always uses both left-to-right and right-to-left 
evaluation whenever |<*>| is used.
This means that the number of formulas produced is roughly exponential
in the number of nodes in the abstract syntax tree. 
However, since only a few of the interpretation rules make use of 
$\xi$, there are many duplicates.
This blow-up can be avoided in most cases by changing the implementation
of the continuation functor, as shown below.
Now, the number of formulas produced is exponential in the number of 
uses of $\xi$. For some sentences, this may still be a significant 
number, but these are now syntactically different readings.
Some of them may still be logically equivalent, for example if they only differ
in the order of adjacent universal quantifiers.

\subsubsection{A more efficient functor}

%if style /= newcode

%include Inter.lhs

%endif

\section{Related Work}
\label{sec:related-work}

\citet{ranta04:semantics-type-theory}
has a compositional semantics in dependent type theory .
We share the idea of writing interpretation rules on the abstract 
syntax of a type theoretic grammar (GF in both cases).
However, Ranta uses a dependently typed \lcalc where we
use a more pedestrian polymorphic \lcalc.
Also, Ranta handles quantifier scope ambiguity by making the 
grammar ambiguous while keeping the interpretation rules
straightforward.

Compared to \citet{barker02:continuations-quantification}, we have a more elegant way of writing
interpretation rules, since the applicative functor formulation
and control operators take care of the continuation plumbing.
Also, we implement non-deterministic evaluation order in a single place,
the definition of |<*>|, rather than having to add multiple interpretations
for each syntactic construct.

The idea of using a applicative functor for hiding the continuation
plumbing is very similar to Shan's \shortcite{shan01:monads-natural-language} 
use of monads for the same purpose,
but the restrictions of a monad does not allow the non-deterministic evaluation
order that we need.

In contrast to \citet{blackburn05:compsem}, we are using
a an explicit abstract syntax, which makes it
easier to change the surface language or the semantics independently.
In particular, this makes it possible for us to construct a multilingual
semantics.


%}



%if not report

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\end{document}

%endif