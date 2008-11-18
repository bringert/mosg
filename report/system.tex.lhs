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
to parse input in any of 10 languages, and interprets it 
using a compositional semantics based on continuations.
\end{abstract}

%endif

\section{Introduction}

Grammatical Framework (GF)~\cite{ranta04:gf}.

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



%if not report

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\end{document}

%endif