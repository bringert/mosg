\documentclass{article}

\usepackage[pdftex,bookmarks,unicode]{hyperref}

%include polycode.fmt

%if style /= newcode
% Use sans-serif font for constructors 
%subst conid a     = "\mathsf{" a "}"

%format <*>          = "\varoast"
%format &&&          = "\land"
%format |||          = "\lor"
%format ==>          = "\Rightarrow"
%format ===          = "="
%format =/=          = "\neq"
% FIXME: need better <=*=> symbol
%format <=*=>        = "\bothways"
%format thereIs      = "\exists"
%format forAll       = "\forall"
%format neg          = "\lnot"
%endif

\newcommand{\bothways}{\ensuremath{\mathaccent\varoast\longleftrightarrow}} 

\title{Delimited Continuations, Applicative Functors and Natural Language Semantics}

\author{
  Bj\"{o}rn Bringert \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology and University of Gothenburg\\
  \texttt{bringert{\char64}cs.chalmers.se}}

\begin{document}

\maketitle

\begin{abstract}

\end{abstract}

\section{Introduction}

> import FOL
> import Inter
> import Input
> import Control.Applicative (pure,(<*>))
> import Data.List (nub)
> import Text.PrettyPrint.HughesPJ hiding (char)

\section{Abstract syntax}

> data S = PredVP NP VP
%if style == newcode
>   deriving Show
%endif

> data RS = RelVP VP 
%if style == newcode
>   deriving Show
%endif

> data VP = ComplV2 V2 NP
>         | UseV V
%if style == newcode
>   deriving Show
%endif

> data NP = DetCN Det CN
>         | Everyone
>         | Someone
>         | John
%if style == newcode
>   deriving Show
%endif

> data Det = Every | A
%if style == newcode
>   deriving Show
%endif

> data CN = UseN N
>         | ComplN2 N2 NP 
>         | RelCN CN RS
%if style == newcode
>   deriving Show
%endif

> data V = Sleep
%if style == newcode
>   deriving Show
%endif

> data V2 = Love
%if style == newcode
>   deriving Show
%endif

> data N = Man | Woman | Shake
%if style == newcode
>   deriving Show
%endif

> data N2 = Owner
%if style == newcode
>   deriving Show
%endif


\section{Semantics}

> iS :: S -> I Prop
> iS (PredVP np vp) = iNP np <=*=> iVP vp

> iRS :: RS -> I (Exp -> Prop)
> iRS (RelVP vp) = iVP vp

> iVP :: VP -> I (Exp -> Prop)
> iVP (ComplV2 v2 np) = iV2 v2 <*> iNP np
> iVP (UseV v) = iV v

> iNP :: NP -> I ((Exp -> Prop) -> Prop)
> iNP (DetCN det cn) = iDet det <*> iCN cn
> iNP Everyone = cont $ \c -> forAll  (\x -> c ($ x))
> iNP Someone  = cont $ \c -> thereIs (\x -> c ($ x))
> iNP John = pure ($ FOL.Const "john")

> iDet :: Det -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
> iDet Every  = cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
> iDet A      = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

> iCN :: CN -> I (Exp -> Prop)
> iCN (UseN n) = iN n
> iCN (ComplN2 n2 np) = iN2 n2 <*> iNP np
> iCN (RelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs

> iV :: V -> I (Exp -> Prop)
> iV Sleep = pure (\x -> Pred "sleep" [x])

> iV2 :: V2 -> I (((Exp -> Prop) -> Prop) -> Exp -> Prop)
> iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))

> iN :: N -> I (Exp -> Prop)
> iN Man   = pure (\x -> Pred "man"   [x])
> iN Woman = pure (\x -> Pred "woman" [x])
> iN Shake = pure (\x -> Pred "shake" [x])

> iN2 :: N2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iN2 Owner = pure (\o x -> o (\y -> Pred "owner" [x,y]))


\section{Testing}

> utt1 = PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman)))

> utt2 = PredVP (DetCN A (UseN Man)) (ComplV2 Love (DetCN Every (RelCN (UseN Woman) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))))

> utt3 = PredVP John (ComplV2 Love (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))))

> utt4 = PredVP (DetCN Every (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Woman)))))) (UseV Sleep)

> test = mapM_ print . nub . retrieve . iS


\bibliographystyle{plainnat}
\bibliography{bringert-bibliography.bib}

\end{document}