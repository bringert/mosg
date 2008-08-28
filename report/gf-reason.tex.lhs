% -*- Mode: LaTeX; coding:utf-8 -*-

\documentclass[a4paper,twoside,openany]{report}
\usepackage[pdftex,bookmarks,unicode]{hyperref}
\usepackage{natbib}

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

\section{Introduction}

This report presents a natural language question answering system
which combines Grammatical Framework grammars,
compositional semantics and automated reasoning tools.
We first define a first-order compositional semantics for a
large fragment of the language-independent
Grammatical Framework resource grammar API.
This lets us assign meaning to sentences in any of the
(currently 11) languages in the resource grammar library.
Existing automated reasoning tools for first-order logic
are then used to combine facts and answer questions about them.

\begin{figure}
FIXME: put overview picture here
\caption{Overview of the question answering system.}
\label{fig:qa-overview}
\end{figure}

An over view of the complete system is shown in Figure~\ref{fig:qa-overview}.


Innovations: 

First-order logic semantics for a substantial fraction of the 
GF resource grammar API.

Continuations for scope ambiguities.

Handling of ambiguous inputs in reasoning.

Use of answer predicates for extracting answers to wh-questions
from proofs.

%{
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

%}

\section{Delimited Continuations, Applicative Functors and Natural Language Semantics}

Philosophy: don't push everything into the lexicon.
Lexical semantics should be as simple as possible, since there are more
lexicon entries than syntactic constructs.
Moving more to the semantics of syntactic constructs makes the semantics
easier to understand.
Example: adjectives as CN -> CN makes predicative use of adjectives
awkward.

We present the semantics for a progression of increasingly large fragments
of the example abstract syntax shown in Figure~\ref{fig:Toy-gf}.

\subsection{Simplest fragment}

The first fragment only contains transitive and intranstive verbs, and a single proper 
name, along with the neccessary predication and complementation rules.
The semantics is implemented as a Haskell~\citep{haskell98} program.

> iS :: S -> Prop 
> iS (PredVP np vp) = (iVP vp) (iNP np)

> iNP :: NP -> Exp
> iNP (UsePN pn) = iPN pn

> iVP :: VP -> (Exp -> Prop)
> iVP (UseV v) = iV v
> iVP (ComplV2 v2 np) = (iV2 v2) (iNP np)

> iPN : PN -> Exp
> iPN John_PN = Const "John"

> iV :: V -> (Exp -> Prop)
> iV Walk_V = \x -> Pred "walk" [x]

> iV2 :: V -> (Exp -> Exp -> Prop)
> iV2 Love_V = \x y -> Pred "love" [x,y]

This lets use handle the sentences ``John walks'' and ``John loves John''.

\subsection{Adding determiners}

\subsection{Quantifier scope ambiguity}

\subsection{Scope islands}

%{

%include semantics.fmt

%include examples/toy/SemToy.tex.lhs

%}

\section{Ambiguity and Answers}

\section{Evaluation}

\section{Future Work}

\subsection{Background Knowledge}

So far, we have only used the system with small sets of facts
provided in natural language. One way to make this system 
useful for real-world tasks would be to give the system 
background knowledge, for example by importing a 
knowledge base such as OpenCyc or ConceptNet.
Lexical knowledge bases such as WordNet could also be used,
as is done by Bos.

\subsection{Robustness}

The current system relies solely on GF's parsing 

\subsection{Multilingual Reasoning}

While the current system works in several languages,
there are no semantic relations between words
in different languages. This means that we cannot, for example,
answer questions about facts given in another language.

\section{Conclusions}

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\clearpage

\appendix

% include Sem.tex.lhs

\end{document}
