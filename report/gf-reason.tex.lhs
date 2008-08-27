% -*- Mode: LaTeX; coding:utf-8 -*-

\documentclass[a4paper,twoside,openany]{report}
\usepackage[pdftex,bookmarks,unicode]{hyperref}
\usepackage{natbib}

%include polycode.fmt

%if style /= newcode
% Use sans-serif font for constructors 
%subst conid a     = "\mathsf{" a "}"

%format <*>          = "\varoast"
%format &&&          = "\land"
%format |||          = "\lor"
%format ==>          = "\Rightarrow"
%format <=>          = "\Leftrightarrow"
%format ===          = "="
%format =/=          = "\neq"
%format thereIs      = "\exists"
%format forAll       = "\forall"
%format neg          = "\lnot"

%format I            = "\mathcal{I}"
%endif

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

Present complete system.

\begin{figure}
FIXME: put overview picture here
\caption{Overview of the question answering system.}
\label{fig:qa-overview}
\end{figure}

Innovations: 

First-order logic semantics for a substantial fraction of the 
GF resource grammar API.

Continuations for scope ambiguities.

Handling of ambiguous inputs in reasoning.

Use of answer predicates for extracting answers to wh-questions
from proofs.

\section{Grammatical Framework (GF)}

Grammatical Framework (GF)~\cite{ranta04:gf} is a type-theoretical 
grammar formalism. GF makes a distinction between abstract and concrete syntax.
GF supports grammar libraries.

\subsection{GF Resource Library}

The GF Resource Library~\cite{ranta08:resource-library} implements
a large fragment of the syntax and complete morphologies for
(currently) eleven languages. The grammars for all languages
have a common abstract syntax, though there are also some
language-specific extensions.


\section{Compositional Semantics with Continuations}

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
