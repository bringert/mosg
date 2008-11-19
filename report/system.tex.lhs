% -*- Mode: LaTeX; coding:utf-8 -*-

%if not report

\documentclass[a4paper]{article}
\usepackage{natbib}
\usepackage{verbatim}
\usepackage{graphicx}
\usepackage{xspace}

\let\cite=\citep
\let\shortcite=\citeyearpar
\newcommand{\lcalc}{$\lambda$-calculus\xspace}

\title{Natural Language Reasoning with GF and FOL}

\author{
  Bj\"{o}rn Bringert \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology and University of Gothenburg\\
  \texttt{bringert{\char64}chalmers.se}}

\begin{document}

\maketitle

\begin{abstract}
We present a multilingual natural language reasoning system
which translates natural language facts and questions to first-order logic,
and uses off-the-shelf automated reasoning tools to reason about them.
The system uses the Grammatical Framework (GF) resource grammar library
to parse input in any of 10 languages.
The semantic interpretation is compositional,
with delimited continuations with non-deterministic evaluation
order.
\end{abstract}

%endif

\section{Introduction}


\begin{figure}
\includegraphics[width=\textwidth]{figures/high-level-overview}
\caption{High-level overview of the system.}
\label{fig:qa-high-level-overview}
\end{figure}

\begin{figure}
\includegraphics[width=\textwidth]{figures/screenshot}
\caption{Screenshot of the system.}
\label{fig:qa-screenshot}
\end{figure}


\section{Parsing}

Input is parsed with a Grammatical Framework (GF)~\cite{ranta04:gf}
grammar. The grammar is based on the GF Resource Grammar library.
The result of the parsing is an abstract syntax tree, which 
is largely language-independent.
Multiple trees can be returned for a sinlge input, since the
grammar is ambiguous.

\section{Interpretation}

Each abstract syntax tree is interpreted as a set of
logical forms. This set may be empty, in case one of the constructions
in the tree does not have an interpretation in the semantics.
The set may also have more than one element, in case there are multiple
interpretations of the abstract syntax tree, for example because of
quantifier scope ambiguities.

The possible logical forms are |stm(p)|, |ynq(p)| and |whq(X,q)|,
where |p| is a closed formula in first-order logic,
and |q| is a formula where the variable |X| is free.

The interpretation is compositional and continuation-based.
Continuations are used to make it possible to add quantifiers at the top
level.
Delimited continuations are used to create scope islands.
Non-deterministic evaluation order is used to generate
alternative quantifier scopes.


\section{Reasoning}


%if not report

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\end{document}

%endif