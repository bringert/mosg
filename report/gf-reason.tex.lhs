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


\begin{figure}
FIXME: put overview picture here
\caption{Overview of the question answering system.}
\label{fig:qa-overview}
\end{figure}



\bibliographystyle{plainnat}
\bibliography{bringert-bibliography}

\clearpage

\appendix

% include Sem.tex.lhs

\end{document}
