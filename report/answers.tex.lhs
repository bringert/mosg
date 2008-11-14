% -*- Mode: LaTeX; coding:utf-8 -*-

%if not report

\documentclass{article}

\usepackage{url}
\usepackage{multirow}
\usepackage{natbib}
\usepackage{verbatim}

\let\cite=\citep
\let\shortcite=\citeyearpar

\title{Answers and Ambiguity in Automated Reasoning about Natural Language}

\author{Bj\"{o}rn Bringert \and Koen Claessen \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology \\ G\"{o}teborg University \\
  \texttt{\{bringert,koen\}{\char64}chalmers.se}}

\begin{document}

\maketitle

\begin{abstract}
We explore how standard first-order reasoning tools can be used
for natural language inference tasks.
Natural language inference answers questions posed in natural language,
given a text in natural language.
We base our work on an existing system which can interpret natural language 
sentences as formulas in first-order logic, and use existing theorem provers
and model finders as inference back-ends.
We present novel approaches to the following three issues:
(1) We improve the traditional handling of 
\emph{incomplete knowledge}, when definite positive or negative answers cannot be given,
by splitting the ``unknown'' answer into
several, such as ``possibly yes'', ``possibly no'', ``possibly either''.
(2) In the case of \emph{ambiguity}, when statements or questions 
have multiple interpretations, we introduce two complementary solutions,
an optimistic and a pessimistic one.
(3) For \emph{wh-questions}, questions that use ``what'', ``who'' etc.,
and thus need more than a simple yes/no answer, we use 
\emph{answer predicates} to extract concrete instantiations of the question variable.
Finally, we evaluate our proposed solutions on the FraCaS natural language inference test suite.
\end{abstract}

%endif

\section{Introduction}

Consider building a system that can answer natural language questions 
based on facts also given in natural language. For example,
given the facts ``John is a man'' and ``John loves Mary'',
it should be able to given the answer ``yes'' to the question
``Is there a man who loves Mary?''.

FIXME: example applications

We are interested in the question of how 
existing automated reasoning tools can be used to answer
such questions. What problems should we
present the reasoning tools with? And how should we interpret 
their answers?
One problem is that sentences in natural language 
often are ambiguous. Ambiguity can occur on many levels,
including lexical, syntactic, and scope ambiguities. 
Another problem is how to deal with the undecidability
of strong logics.

We will approach these questions in the setting of a 
specific system which interprets natural language
sentences as formulas in first-order logic.
The system uses a grammar for a large fragment of 
English (and other languages, but we will use English 
for all examples), and provides interpretations
for each construct in the grammar.
Not all constructs in a complete natural language can be given 
interpretations in first-order logic. One well-known example
is generalized quantifiers such as ``most''\cite{barwise81:generalized-quantifiers}. 
The book by Blackburn and Bos~\shortcite{blackburn05:compsem}
gives a good introduction to the problem of interpreting
natural language in first-order logic (and also reasoning about it with
off-the-shelf tools).

The facts and the question, 
will each have one or more first-order interpretations.
How can we convert this into problems for theorem provers and
model builders? 
And how do we interpret the output of these tools?

We first consider the subproblem where every sentence has exactly 
one interpretation, and then generalize this to ambiguous
sentences. We will not arrive at a single correct method for
handling ambiguities. Instead, we map out the available options,
and discuss their ramifications.

We will mostly be concerned with Yes/No questions, but we will
also touch on how to treat wh-questions.



\section{Unambiguous Problems}

We now consider the case where we are given 
zero or more facts $F_i$, and a yes/no question $Q$,
and each of them can be given an umambiguous 
interpretation in first order logic.
We use the $*$ operator to denote interpretation.
From the interpretation of the facts, we form a \emph{theory},
$T = \bigwedge F_i*$. That is, the theory is the conjunction
of the interpretations of the given facts.
We will refer to the interpretation of the question
as the \emph{hypothesis} $H$.
For example, consider the facts $F_1 =$ ``John is a man.'' and
$F_2 =$ ``John loves Mary.'', and the question 
$Q = $ ``Is there a man who loves Mary?''.
Let us say that we get the following fact interpretations:
$F_1* = man(john)$ and $F_2* = love(john,mary)$.
The theory is then $T = F_1* \land F_2* = man(john) \land love(john,mary)$.
The hypothesis (question interpretation) is then
$H = Q* = \exists X. man(X) \land love(X,mary)$.

Given a theory and a hypothesis, what can we find out
about them using automated reasoning tools? We will consider
two kinds of tools: theorem provers and model builders.

When given a formula, a theorem prover can either tell us that
the formula is not satisfiable, or it can fail to deliver an answer
(some provers can also sometimes conclude that the formula
is satsifiable).
Analoguously, a model builder can tell us that the given formula 
is satisfiable, or it can fail.
When used together, a theorem prover and a model builder
can give us one of three answers:
the formula is satisfiable, the formula is not satisfiable,
``don't know''. If the model builder fails, the theorem prover
is used to check for unsatisfiability, or they could be run in 
parallel. If the tools are sound, they will never give conflicting 
answers.

FIXME: show decision tree.

But what formula should we give to the tools? 
Consider checking $T \land \lnot H$ 
(or equivalently, $\lnot (T \Rightarrow \lnot H)$).
If this is insatisfiable, we know that the
hypothesis follows from the theory, that is, the
answer is ``yes''.
We may also want to find out whether the hypothesis
is contradicted by the theory. We then check 
$T \land H$ (or equivalently, $\lnot (T \Rightarrow H)$), 
and if this is insatisfiable,
we can give the answer ``no''.
If we can't prove either ``yes'' or ``no'',
there is no definite answer that we can give.

But by using the results from the
model builder, we can do better than ``I don't know''.
If the model builder reports that $T \land H$
is satisfiable, then we know the hypothesis can be true.
If $T \land \lnot H$ is satisfiable, the hypothesis can be false.

Put together, these tests produce the possible outcomes shown in 
Figure~\ref{fig:answers}. The top row represents ``possibly no'',
the middle row ``don't know about no'', and the bottom
``not no''. The leftmost column is ``possibly yes'',
the center column is ``don't know about yes'',
and the rightmost column is ``not yes''.
This produces 7 distinct possible final answers 
(not 9, since we ignore ``unknown'' answers from the
model builder when the theorem prover gives
a definite one). $H$
One interesting case is the bottom right corner,
where both $T \land H$ and $T \land \lnot H$
are insatisfiable, or in other words,
both the hypothesis and its negation can be proven
using the theory. The only way that this can happen is if the
theory is inconsistent.

\begin{figure}
\begin{tabular}{rr||c||c||c||}
& & \multicolumn{3}{c}{$T \land H$ satisfiable?} \\
& & yes & unknown & no \\
\cline{2-5}
\multirow{3}{*}{$T \land \lnot H$ satisfiable?} & yes & 1: maybe yes, maybe no & 2: maybe no & 3: no \\
& unknown & 4: maybe yes & 5: don't know & 6: no \\
& no & 7: yes & 8: yes & 9: T inconsistent \\
\end{tabular}
\caption{Answer matrix for unambiguous inputs.}
\label{fig:answers}
\end{figure}

\subsection{Examples}

Figure \ref{fig:answer-examples} shows 
example problems for each of the 9 cells in Figure~\ref{fig:answers}.

\begin{figure}
\begin{tabular}{||r||l||l||l||l||}
& Facts & Question & T & H \\
1 & ``John loves Mary.'' & ``Does Mary love John?'' & $love(john,mary)$ & $love(mary,john)$ \\
2 & FIXME \\
3 & ``John doesn't love Mary'' & ``Does John love Mary?'' & $\lnot love(john,mary)$ & $love(john,mary)$ \\
4 & FIXME \\
5 & FIXME \\
6 & FIXME \\
7 & ``John loves Mary.'' & ``Does John love Mary'' & $love(john,mary)$ & $love(john,mary)$ \\
8 & FIXME \\
9 & ``John loves Mary. John doesn't love Mary.'' & ``Does John love Mary?'' & $love(john,mary) \land \lnot love(john,mary)$ & $love(john,mary)$ \\  \\
\end{tabular}
\caption{Example problems for the 9 cells in Figure~\ref{fig:answers}.}
\label{fig:answer-examples}
\end{figure}

\section{Ambiguous Problems}

In many cases, both the facts and the question can be 
ambiguous, that is, it may have more than one 
semantic interpretation .

\begin{description}
\item[Lexical ambiguity]
Because of synonymy and polysemy, words may have several
possible meanings. For example,
``John sees a bank.'' could be mean
$\exists x. bank_{financial institution}(x) \land see(john,x)$
or 
$\exists x. bank_{edge of a river}(x) \land see(john,x)$.
We could try to get around this problem by simply interpreting
the sentence as $\exists x. bank(x) \land see(john,x)$,
where $bank(x)$ means ``any kind of bank''. 
If we want to introduce background knowledge about the different
kinds of banks, we could use
$forall x. bank(x) \Rightarrow bank_{financial institution}(x)
\lor bank_{edge of a river}(x)$.

\item[Syntactic ambiguity]
Depending on the grammar, a single sentence may be assigned
several different syntax trees. This normally results in different
interpretations.
Prepositional phrase attachment (PP attachment) is a common
source of syntactic ambiguity.
For example, ``John sees a girl with a telescope.'' could mean
$\exists x,y. girl(x) \land telescope(y) \land with(x,y) \land see(john, x)$
or
$\exists x,y. girl(x) \land telescope(y) \land see(john, x) \land with(john,y)$.
Another kind of syntactic ambiguity: ``They saw her duck''
\footnote{from Stampe with collaboration of Patton, according to 
Zwicky and Sadock\shortcite{zwicky75:ambiguity-tests}}.
\item[Interpretation ambiguity]
Even if a sentence can be assigned an umambiguous syntactic structure,
the scope of quantifiers may still be ambiguous.
For example,
``Every man loves a woman.'' could mean
$\forall x. man(x) \Rightarrow \exists y. woman(y) \land love(x,y)$
or
$\exists y. woman(y) \land \forall x. man(x) \Rightarrow love(x,y)$.
The difference between these two is that in the latter, all men love
the same woman.
\item[Negation]
The sentence ``I don't think she's bald''
has two readings, ``It's not the case that I think that she is bald'',
and ``I think she's not bald'' \cite{zwicky75:ambiguity-tests}.


\end{description}

For more on ambiguities,see \cite{zwicky75:ambiguity-tests}.

We use the simple definition that a sentence is ambiguous
if it receives more than one interpretation when using
whatever interpretation method we have chosen.
An example that is ambiguous with one method,
may instead be underspecified with another.

Ambiguities can also be artifacts
of the imperfect interpretation machinery used.

If the inputs are ambiguous, what do we ask our reasoning tools?
And how do we interpret their responses?

One possibility is to remove fact interpretations that are inconsistent
with other facts. We could also remove interpretations that are not
informative with respect to other facts. See \cite{blackburn05:compsem}.

Interpretations can be equivalent. Only one interpretation in each
equivalence class needs to be kept.
Should this be done under the current theory, or 
stand-alone?

One of the interpretations of an input may imply another of the 
interpretations. One possibility is to keep only the stronger of the two needs to
be kept.
Should this be done under the current theory, or 
stand-alone?

\subsection{Pessimistic Problem Interpretation}

If we want to make sure that all ``yes'' and ``no'' answers are correct
(high precision),
we can make the pessimistic choice: $T = \bigvee T_i$ 
and $H = \bigwedge H_i$. In other words,
for a positive answer, we require that every interpretation of the
hypothesis follows from every interpretation of
the facts. 
This will lead to many ``maybe'' answers.

This allows ambiguous inputs to be clarified by later inputs.
For example, if the first input is ``every man loves a woman'',
we get 
$(\forall x. man(x) \Rightarrow \exists y. woman(y) \land love(x,y))
\lor (\exists y. woman(y) \land \forall x. man(x) \Rightarrow love(x,y))$.
If the next input is ``there is no woman whom every man loves'',
(interpreted as $\lnot \exists y. woman(y) \land \forall x. man(x) \Rightarrow love(x,y)$,
unambiguously since relative clauses are \emph{scope islands}),
the result is that only the first reading of the first input 
remains possible.

What about when checking for a negative answer?

\paragraph{Example ``yes'' answer}

``John is old. John sees a bank.'',
``Is John old?'',
$T = (old(john) \land \exists x. bank_{financial institution}(x) \land see(john,x))
\lor (old(john) \land \exists x. bank_{edge of a river}(x) \land see(john,x))$.
$H = old(john)$.

\paragraph{Example ``no'' answer}

As above, with question ``Is John not old?''.


\paragraph{Example ``maybe'' answer}

For example:
``John sees a bank'', ``Does John see a bank?'',
$T = (\exists x. bank_{financial institution}(x) \land see(john,x))
\lor (\exists x. bank_{edge of a river}(x) \land see(john,x))$.
Here both $T \land H$ and $T \land \lnot H$ are satisfiable,
giving us a ``maybe yes, maybe no'' answer.

What about inconsistent question interpretations?
Find example.

\subsection{Optimistic Problem Interpretation}

In order to be able to answer as many questions as possible
(high recall), we can instead make an optimistic choice:
$T = \bigwedge T_i$ and $H = \bigvee H_i$. Here, we
only require that some interpretation of the hypothesis
follows from some interpretation of the facts.

\paragraph{Example ``yes'' answer}

``John saw her duck'', ``Did John see a duck?'',
a possible interpretation is
$T = (\exists X. duck(X) \land her(X) \land see(john,X))
\land (\exists E. see(John,E) \land subject(X,she) \land action(X,duck))$,
$H = \exists X. duck(X) \land see(john,X)$

``John sees a bank'', ``Does John see a bank?'',

\paragraph{Example ``no'' answer}




\paragraph{Example ``maybe'' answer}

``John is a man'', ``Is John old?''.


One serious problem of the optimistic strategy is that
it is prone to inconsistency. Consider the example above where the inputs 
are ``every man loves a woman'', and ``there is no woman whom every man loves''.
While this has the result of clarification in the pessimistic strategy,
it produces an inconsistency in the optimistic one.


\subsection{Other Strategies}

Pessimistic facts, optimistic questions? Could be useful for
lexical ambiguities, ``John sees a bank'' is interpreted as 
disjunction, and ``Does John see a bank'' also as disjunction.
The advantage is that the facts and the questions are interpreted
in the same way.

\subsection{Pessimistic vs Optimistic}

The pessimistic interpretation is appropriate when we only want to deliver
definite answers when they are certain. Examples of such applications could
include safety critical or business critical systems.

The optimistic interpretation is useful when we want to get as many 
definite answers as possible. This could for example be used to identify
candidates for subsequent human or statistical assessment.

\section{Answering wh-questions}

We would also like to be able to answer questions such as ``Whom does John love?''.
The question can be interpreted as $exists X. love(john,X)$. We would like to prove this,
and get the possible values of $X$. Some theorem provers support an 
\emph{answer predicate}, which lets us get at this information.

Given a theory $T$ and a wh-question interpretation $\exists X. \phi$,
we ask the theorem prover to disprove 
$\lnot \exists X. \phi \land \$answer(X)$.

In an example such as ``John loves Mary.'', ``Whom does John love?'',
we can get the answer $\$answer(mary)$, which is is easily presented
as the answer ``Mary'', or even ``John loves Mary''.
However, consider ``John loves a woman.'', ``Whom does John love?''.
We would like to get the answer ``a woman'', but that is not quite as easy.
The theorem prover will report $\$answer(c)$, where $c$ is some Skolem constant.
We could then examine the proof, to find out more about $c$.
Once we have a logic formula with information about $c$, we need to 
generate a natural language answer from it. This is a non-trivial problem.

\section{Experimental Results}

We have evaluated the different question answering strategies
using the FraCaS semantic test suite \cite{cooper96:fracas-test-suite}.
The test suite consists of a set of 346 problems, each of which has 1 to five
premises and one question. The problems are annotated with ``yes'', ``no'',
or ``don't know''.
Some problems are degenerate, and some have complex answers.
We used the machine readable version of the test suite as
prepared by 
Bill MacCartney\footnote{\url{http://nlp.stanford.edu/~wcmac/downloads/fracas.xml}}.

FIXME: do evaluation

\section{Future Work}

Can we use the models for anything?

\section{Related Work}

Blackburn and Bos~\shortcite{blackburn05:compsem}.

Bos~\shortcite{bos07:automated-deduction-nlu}.

LogAnswer~\cite{furbach08:loganswer-description}.

\section{Conclusions}

%if not report

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography.bib}

\end{document}

%endif