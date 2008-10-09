% -*- Mode: LaTeX; coding:utf-8 -*-

\documentclass[a4paper,twoside,openany]{report}
\usepackage[pdftex,bookmarks,unicode]{hyperref}
\usepackage{natbib}
\usepackage[pdftex]{graphicx}

\let\cite=\citep
\let\shortcite=\citeyearpar

%include polycode.fmt

%% Tell included files (e.g. semantics.tex.lhs) that they are not stand-alone.
%let report = True


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
in the Montague \cite{montague73:ptq} tradition.

We use continuations to handle scope 
ambiguities~\cite{barker02:continuations-quantification},
and delimited continuations for scope islands
\cite{shan04:delimited-continuations}.

To simplify the implementation, we use applicative 
functors~\cite{mcbride07:applicative}. Shan~\shortcite{shan01:monads-natural-language}
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



\chapter{Delimited Continuations, Applicative Functors and Natural Language Semantics}
\label{chapter:semantics}

%include semantics.tex.lhs

\chapter{Semantics for the GF Resource Grammar API}
\label{chapter:resource-grammar-semantics}

%include Sem.tex.lhs

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
