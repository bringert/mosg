% -*- Mode: LaTeX; coding:utf-8 -*-

\documentclass[a4paper,twoside,openany]{report}
\usepackage[pdftex,bookmarks,unicode]{hyperref}
\usepackage{natbib}
\usepackage[pdftex]{graphicx}

%include polycode.fmt



\pagestyle{headings}


\title{Natural Language Reasoning with Grammatical Framework}

\author{
  Bj\"{o}rn Bringert \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology and University of Gothenburg\\
  \texttt{bringert{\char64}chalmers.se}}

\begin{document}

\maketitle

\begin{abstract}


\end{abstract}

\tableofcontents

\chapter{Introduction}
\label{chapter:introduction}

This report presents a number of ideas about practical
logic-based natural language semantics and reasoning.
These ideas form the basis for a natural language
reasoning system which we also describe and evaluate.

The report is organized into a number of chapters:

\begin{description}

\item[Chapter~\ref{chapter:introduction}] 
gives overview and background.

\item[Chapter~\ref{chapter:semantics}]
describes NL first-order semantics with non-deterministic continuation functor.
Includes a section on Haskell implementation.

\item[Chapter~\ref{chapter:resource-grammar-semantics}]
defines the semantics of a significant fragment of the GF Resource Grammar API,
which lets us assign meaning to many sentences in any of the
(currently 11) languages in the resource grammar library.

\item[Chapter~\ref{chapter:ambiguity-answers}]
describes an approach to dealing with ambiguity and
wh-questions when answering natural language questions 
using existing automated reasoning tools.

\item[Chapter~\ref{chapter:system-description}]
provides a detailed description on a complete question
answering system based on the ideas presented in the earlier 
chapters.

\item[Chapter~\ref{chapter:evaluation}]
discusses the results of running the system on the FraCaS semantic test suite.

\end{description}

\section{System overview}

While the ideas presented in the first few chapters can be
independently useful, it maybe helpful to picture them in the larger
context in which the developed.
Thus, we will first give a brief overview of 
the complete question answering system which is
the final product of this work. A more detailed system description
can be found in Chapter~\ref{chapter:system-description}.
The system accepts two kinds of inputs, facts and questions,
both of which are expressed in natural language,
and attempts to provide answers to any questions 
based on the facts that it has been given.
A high-level overview of the system is shown in 
Figure~\ref{fig:qa-high-level-overview}.

\begin{figure}
\includegraphics[width=\textwidth]{figures/high-level-overview}
\caption{High-level overview of the question answering system.}
\label{fig:qa-high-level-overview}
\end{figure}

\section{Features}

This work combines a number of existing ideas in a novel way,
ands adds a few new ones as well.

We have written a first-order logic semantics for a substantial fraction of the 
GF resource grammar API~\cite{ranta08:resource-library}.

This is a compositional semantics based on typed lambda calculus,
in the Montague \citep{montague73:ptq} tradition.

We use continuations for to handle scope 
ambiguities~\cite{barker02:continuations-quantification},
amd delimited continuations for scope islands
\cite{shan04:delimited-continuations}.

To simplify the implementation, we use applicative 
functors~\cite{mcbride07:applicative}. Shan~\cite{shan01:monads-natural-language}
has used monads for a similiar effect, but this does not allow 
the non-determinism that we want.

We describe a novel way of handling ambiguous inputs in reasoning.

We use answer predicates for extracting answers to wh-questions
from proofs.

\subsection{Example interactions}

Barber paradox, with several formulations.

``the barber is a man who shaves only all men who do not shave themselves''

``the barber shaves all and only those who do not shave themselves''

``the barber shaves everyone, except those who shave themselves''

Consistency checking when adding facts not only helps reduce ambiguity, but 
also saves us from having an inconsistent knowledge base.



%{
%if style /= newcode
%include gf.fmt

\section{Grammatical Framework (GF)}

Grammatical Framework (GF)~\cite{ranta04:gf} is a type-theoretical 
grammar formalism. GF makes a distinction between abstract and concrete syntax.
GF supports grammar libraries.

\subsection{Example Grammar}

In order to introduce GF, and to give us an example syntax to work with 
in the following section, we present an example gramamr for a fragment
of English. The abstract syntax shown in Figure~\ref{fig:Toy-gf}
defines categories (|cat|) and functions (|fun|).
The concrete syntax shown in Figure~\ref{fig:ToyEng-gf} defines
the linearization type (|lincat|) of each abstract syntax category,
and the linearization (|lin|) of each abstract syntax function.
In this simple example, no inflection, agreement or other
complex morphosyntactic features are needed to implement the English
concrete syntax. However, GF does allow more sophisticated linearization rules,
with records, finite functions and algebraic data types, which can be used to 
implement more complex grammars without changing the abstract syntax.

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

GF can be used to create multilingual grammars by associating multiple
concrete syntaxes with a single abstract syntax.

\subsection{GF Resource Library}

The GF Resource Library~\cite{ranta08:resource-library} implements
a large fragment of the syntax and complete morphologies for
(currently) eleven languages. The grammars for all languages
have a common abstract syntax, though there are also some
language-specific extensions.
The reasoning system uses the resource grammar library,
along with some minor extensions.

%endif
%}


%{
%include semantics.fmt

\chapter{Delimited Continuations, Applicative Functors and Natural Language Semantics}
\label{chapter:semantics}

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

\subsection{Preliminaries}

In order to write haskell functions over GF abstract syntax terms, we first
need a Haskell representation of such terms. The code below has been
automatically generated by GF from the abstract syntax module 
in Figure~\ref{fig:Toy-gf}.

FIXME: do we really need to show the generated data types here?
If so, how to include them automatically? And what about the G prefix?

Simplification rules.

\subsection{Fragment 1: Basics}

The first fragment only contains transitive and intranstive verbs, and a single proper 
name, along with the neccessary predication and complementation rules.
The semantics is implemented as a Haskell~\citep{haskell98} program.

%if sem_toy_1_code || sem_toy_2_code || style /= newcode

%if style == newcode

> import Toy
> import FOL
> import TestToy

%endif

We first provide straightforward semantics for the lexical items.

> iPN :: PN -> Ind
> iPN John = Const "John"
> iPN Mary = Const "Mary"
> iPN Bill = Const "Bill"

> iV :: V -> (Ind -> Prop)
> iV Walk = \x -> Pred "walk" [x]

> iV2 :: V2 -> (Ind -> Ind -> Prop)
> iV2 Eat   = \x y -> Pred "eat" [x,y]
> iV2 Love  = \x y -> Pred "love" [x,y]

> iN :: N -> (Ind -> Prop)
> iN Man     = \x -> Pred "man" [x]
> iN Woman   = \x -> Pred "woman" [x]
> iN Burger  = \x -> Pred "burger" [x]

> iN2 :: N2 -> (Ind -> Ind -> Prop)
> iN2 Owner = \x y -> Pred "owner" [x,y]

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
Barker~\citep{barker02:continuations-quantification} notes
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

> test1 = test (eval . iS) "a man who loves every woman eats a burger"

%endif


This is a representation of a non-deterministic continuation functor.

> type Cont o a = [(a -> o) -> o]

The |pure| function lifts a pure value into the functor.

> pure :: a -> Cont o a
> pure a = [\c -> c a]

The |<*>| operator performs lifted function application.

> (<*>) :: Cont o (a -> b) -> Cont o a -> Cont o b
> xs <*> ys = concat [[  \c -> x (\f -> y (\a -> c (f a))),
>                        \c -> y (\a -> x (\f -> c (f a)))]
>                        | x <- xs, y <- ys]

Runs a compuation and retrive all posible results.

> eval :: Cont o o -> [o]
> eval x = [y (\f -> f) | y <- x]

The same but for single-argument functions.

> eval' :: Cont o (a -> o) -> [a -> o]
> eval' x = [\e -> y (\f -> f e) | y <- x]

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

The |reset| function for creating delimited continuation computations.

> reset :: Cont o o -> Cont p o
> reset x = [\c -> c y | y <- eval x]

Like |reset| but for single argument functions.

> reset' :: Cont o (a -> o) -> Cont p (a -> o)
> reset' x = [\c -> c y | y <- eval' x]

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
> iN Man     = pure (\x -> Pred "man" [x])
> iN Woman   = pure (\x -> Pred "woman" [x])
> iN Burger  = pure (\x -> Pred "burger" [x])

> iN2 :: N2 -> I (Ind -> Ind -> Prop)
> iN2 Owner = pure (\x y -> Pred "owner" [x,y])

> iPN :: PN -> I Ind
> iPN John = pure (Const "John")
> iPN Mary = pure (Const "Mary")
> iPN Bill = pure (Const "Bill")

> iV :: V -> I (Ind -> Prop)
> iV Walk = pure (\x -> Pred "walk" [x])

> iV2 :: V2 -> I (Ind -> Ind -> Prop)
> iV2 Eat   = pure (\x y -> Pred "eat" [x,y])
> iV2 Love  = pure (\x y -> Pred "love" [x,y])

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
in accordance with what Barker~\cite{barker01:integrity} calls the 
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
Barker's~\citep{barker02:continuations-quantification}
treatment of scope islands implicitly uses $reset$ (|reset|).

%if sem_toy_4_code || style /= newcode

> iRS :: RS -> I (Ind -> Prop)
> iRS (RelVP vp) = reset' (iVP vp)

%endif

With this addition, only readings 1 and 6 above are returned.

%}

\section{Related Work}

GF semantics in dependent type theory \cite{ranta04:semantics-type-theory}.


\chapter{Semantics for the GF Resource Grammar API}
\label{chapter:resource-grammar-semantics}

% include Sem.tex.lhs

\section{Future Work}

\subsection{Temporal Reasoning}


\chapter{Ambiguity and Answers}
\label{chapter:ambiguity-answers}

% include answers.tex

\chapter{System Description}
\label{chapter:system-description}

GF interface.

Consistency and informativity checks.

Reasoning interface.

\section{Future Work}

\subsection{Background Knowledge}

So far, we have only used the system with small sets of facts
provided in natural language. One way to make this system 
useful for real-world tasks would be to give the system 
background knowledge, for example by importing a 
knowledge base such as OpenCyc or ConceptNet.
Lexical knowledge bases such as WordNet could also be used,
as is done by Bos.

\subsection{Multilingual Reasoning}

While the current system works in several languages,
there are no semantic relations between words
in different languages. This means that we cannot, for example,
answer questions about facts given in another language.



\chapter{Evaluation}
\label{chapter:evaluation}

FraCaS semantic test suite \cite{cooper96:fracas-test-suite}.
The test suite consists of a set of 346 problems, each of which has 1 to five
premises and one question. The problems are annotated with ``yes'', ``no'',
or ``don't know''.
Some problems are degenerate, and some have complex answers.
We used the machine readable version of the test suite as
prepared by 
Bill MacCartney\footnote{\url{http://nlp.stanford.edu/~wcmac/downloads/fracas.xml}}.

Compare results to \cite{maccartney08:containment,maccartney07:WTEP}.
They only use single-premise problems. Compare that, and that extended with 
default unknown.



\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\end{document}
