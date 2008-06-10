\documentclass{article}

% \usepackage[pdftex,bookmarks,unicode]{hyperref}

%include polycode.fmt

%if style /= newcode
% Use sans-serif font for constructors 
%subst conid a     = "\mathsf{" a "}"

%format <*>          = "\varoast"
%format <|>         = "\ \langle|\rangle\ "
%format &&&          = "\land"
%format |||          = "\lor"
%format ==>          = "\Rightarrow"
%format <=>          = "\Leftrightarrow"
%format ===          = "="
%format =/=          = "\neq"
% FIXME: need better <=*=> symbol
%format <=*=>        = "\bothways"
%format thereIs      = "\exists"
%format forAll       = "\forall"
%format neg          = "\lnot"

%format I            = "\mathcal{I}"
%endif

%\newcommand{\bothways}{\ensuremath{\mathaccent\varoast\longleftrightarrow}} 
\newcommand{\bothways}{\ensuremath{\varoast\!\!\varoast}} 

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

% \section{Introduction}

%if style == newcode

> import FOL
> import Data.List (nub)
> import Text.PrettyPrint.HughesPJ hiding (char)

%endif

\section{Abstract syntax}

\textit{``Every man loves a woman''} \\
|PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman)))|

> data S = PredVP NP VP
%if style == newcode
>   deriving Show
%endif

%if False

> data RS = RelVP VP 
%if style == newcode
>   deriving Show
%endif

%endif

> data VP  = ComplV2 V2 NP | UseV V
%if style == newcode
>   deriving Show
%endif

> data NP  = DetCN Det CN | Everyone | Someone | John
%if style == newcode
>   deriving Show
%endif

> data Det = Every | A
%if style == newcode
>   deriving Show
%endif

> data CN  = UseN N

%if False

>          | ComplN2 N2 NP 
>          | RelCN CN RS

%endif

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

%if False

> data N2 = Owner
%if style == newcode
>   deriving Show
%endif

%endif


\section{Plumbing}

\subsection{Applicative Functors}

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b


\subsection{Applicative Continuation Functor}


> newtype Cont a = Cont { runCont :: (a -> Prop) -> Prop }

> newtype I a = I [Cont a]

> instance Functor I where
>   fmap f x = pure f <*> x

> instance Applicative I where
>   pure a         = I [Cont (\c -> c a)]
>   I xs <*> I ys  = I $ map Cont $ concat
>                        [[  \c -> runCont x  (\f  -> runCont y (\a -> (c (f a)))),
>                            \c -> runCont y  (\a  -> runCont x (\f -> (c (f a))))]
>                          | x <- xs, y <- ys]

> (<|>) :: I a -> I a -> I a
> I xs <|> I ys  = I (xs++ys)

> (<=*=>) :: I (a -> b) -> I a -> I b
> x <=*=> y = (x <*> y) <|> (pure (flip ($)) <*> y <*> x)

> cont :: ((a -> Prop) -> Prop) -> I a
> cont f = I [Cont f]

> retrieve :: I Prop -> [Prop]
> retrieve (I xs) = map (\x -> runCont x id) xs

\section{FOL}

\begin{spec}

data Prop  = Pred String [Exp]
           | Prop &&& Prop
           | Prop ||| Prop
           | Prop ==> Prop
           | Prop <=> Prop
           | neg Prop
           | Prop = Prop
           | forAll (Exp -> Prop)
           | thereIs (Exp -> Prop)
           | true
           | false

data Exp = Const String | Var String

\end{spec}

\section{Semantics}

% In the verb phrase predication rule
% we need to allow quantifiers from either child to have priority.

> iS :: S -> I Prop
> iS (PredVP np vp) = iNP np <=*=> iVP vp

%if False

Relative clauses are scope islands, and thus use |reset|.

> iRS :: RS -> I (Exp -> Prop)
> iRS (RelVP vp) = reset (iVP vp)

%endif

> iVP :: VP -> I (Exp -> Prop)
> iVP (ComplV2 v2 np) = iV2 v2 <=*=> iNP np
> iVP (UseV v) = iV v

% Here, we allow either quantifiers from the determiner,
% or quantifiers from the common noun, e.g.~from
% arguments of an N2, to take priority.

> iNP :: NP -> I ((Exp -> Prop) -> Prop)
> iNP (DetCN det cn) = iDet det <=*=> iCN cn
> iNP Everyone  = cont (\c -> forAll  (\x -> c (\u -> u x)))
> iNP Someone   = cont (\c -> thereIs (\x -> c (\u -> u x)))
> iNP John      = pure (\f -> f (Const "john"))

> iDet :: Det -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
> iDet Every  = cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
> iDet A      = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

> iCN :: CN -> I (Exp -> Prop)
> iCN (UseN n) = iN n

%if False

> iCN (ComplN2 n2 np) = iN2 n2 <*> iNP np
> iCN (RelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs

%endif

> iV :: V -> I (Exp -> Prop)
> iV Sleep = pure (\x -> Pred "sleep" [x])

> iV2 :: V2 -> I (((Exp -> Prop) -> Prop) -> Exp -> Prop)
> iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))

> iN :: N -> I (Exp -> Prop)
> iN Man    = pure (\x -> Pred "man"   [x])
> iN Woman  = pure (\x -> Pred "woman" [x])
> iN Shake  = pure (\x -> Pred "shake" [x])

%if False

> iN2 :: N2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iN2 Owner = pure (\o x -> o (\y -> Pred "owner" [x,y]))

%endif


% \section{Testing}

%if style == newcode

> utt1 = PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman)))

%if False

> utt2 = PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) (UseV Sleep)

> utt3 = PredVP John (ComplV2 Love (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))))

> utt4 = PredVP (DetCN Every (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Woman)))))) (UseV Sleep)

> utt5 = PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) 
>                  (ComplV2 Love 
>                   (DetCN A (RelCN (UseN Woman) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))))

%endif

> test = mapM_ print . nub . retrieve . iS

%endif

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography.bib}

\end{document}