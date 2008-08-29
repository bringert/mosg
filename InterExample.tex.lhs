\documentclass{article}

% \usepackage[pdftex,bookmarks,unicode]{hyperref}

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

\title{Delimited Continuations, Applicative Functors and Natural Language Semantics}

\author{
  Bj\"{o}rn Bringert \\
  Department of Computer Science and Engineering \\
  Chalmers University of Technology and University of Gothenburg\\
  \texttt{bringert{\char64}chalmers.se}}

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

> data RS = RelVP VP 
%if style == newcode
>   deriving Show
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
>          | ComplN2 N2 NP 
>          | RelCN CN RS
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


\section{Plumbing}

\subsection{Applicative Functors}

> infixl 4 <*>, <**>

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f (a -> b) -> f a -> f b

> (<**>) :: Applicative f => f a -> f (a -> b) -> f b
> x <**> y = pure (flip ($)) <*> x <*> y

\subsection{Applicative Continuation Functor}

> data Cont a = Pure a | Cont ((a -> Prop) -> Prop)

> runCont :: Cont a -> ((a -> Prop) -> Prop)
> runCont (Pure x) = \c -> c x
> runCont (Cont r) = r

> instance Functor Cont where
>   fmap f x = pure f <*> x

We have a special case for |Pure f <*> Pure x| to avoid redundant copies
in the implementation of |<*>| for |I|.

> instance Applicative Cont where
>   pure a = Pure a
>   Pure f <*> Pure x  = Pure (f x)
>   x <*> y = Cont $ \c -> runCont x $ \f -> runCont y $ \a -> c (f a)

> newtype I a = I [Cont a]

> instance Functor I where
>   fmap f x = pure f <*> x

> instance Applicative I where
>   pure a             = I [Pure a]
>   I xs <*> I ys      = I [z | x <- xs, y <- ys, z <- app x y]
>     where
>       app :: Cont (a -> b) -> Cont a -> [Cont b]
>       app x@(Cont _) y@(Cont _)  = [x <*> y, y <**> x]
>       app x y                    = [x <*> y]

> cont :: ((a -> Prop) -> Prop) -> I a
> cont f = I [Cont f]

> reset :: Run a => I a -> I a
> reset x = I [pure y | y <- retrieve x]

> retrieve :: Run a => I a -> [a]
> retrieve (I xs) = map run xs

> class Run a where
>   run :: Cont a -> a

> instance Run Prop where
>   run x = runCont x id

> instance Run b => Run (a -> b) where
>   run x = \e -> run (fmap (\f -> f e) x)


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

> iS :: S -> I Prop
> iS (PredVP np vp) = iNP np <*> iVP vp

Relative clauses are scope islands, and thus use |reset|.

> iRS :: RS -> I (Exp -> Prop)
> iRS (RelVP vp) = reset (iVP vp)

> iVP :: VP -> I (Exp -> Prop)
> iVP (ComplV2 v2 np) = iV2 v2 <*> iNP np
> iVP (UseV v) = iV v

> iNP :: NP -> I ((Exp -> Prop) -> Prop)
> iNP (DetCN det cn) = iDet det <*> iCN cn
> iNP Everyone  = cont (\c -> forAll (\x -> c (\u -> u x)))
> iNP Someone   = cont (\c -> thereIs (\x -> c (\u -> u x)))
> iNP John      = pure (\f -> f (Const "john"))

> iDet :: Det -> I ((Exp -> Prop) -> (Exp -> Prop) -> Prop)
> iDet Every  = cont (\c -> forAll (\x -> c (\u v -> u x ==> v x)))
> iDet A      = cont (\c -> thereIs (\x -> c (\u v -> u x &&& v x)))

> iCN :: CN -> I (Exp -> Prop)
> iCN (UseN n) = iN n

Is this a scope island?

> iCN (ComplN2 n2 np) = iN2 n2 <*> iNP np
> iCN (RelCN cn rs) = pure (\ci ri x -> ci x &&& ri x) <*> iCN cn <*> iRS rs

> iV :: V -> I (Exp -> Prop)
> iV Sleep = pure (\x -> Pred "sleep" [x])

> iV2 :: V2 -> I (((Exp -> Prop) -> Prop) -> Exp -> Prop)
> iV2 Love = pure (\u x -> u (\y -> Pred "love" [x,y]))

> iN :: N -> I (Exp -> Prop)
> iN Man    = pure (\x -> Pred "man"   [x])
> iN Woman  = pure (\x -> Pred "woman" [x])
> iN Shake  = pure (\x -> Pred "shake" [x])

> iN2 :: N2 -> I (((Exp -> Prop) -> Prop) -> (Exp -> Prop))
> iN2 Owner = pure (\o x -> o (\y -> Pred "owner" [x,y]))


% \section{Testing}

%if style == newcode

> utt0 = PredVP (DetCN Every (UseN Man)) (UseV Sleep)

> utt1 = PredVP (DetCN Every (UseN Man)) (ComplV2 Love (DetCN A (UseN Woman)))

> utt2 = PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) (UseV Sleep)

> utt3 = PredVP John (ComplV2 Love (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))))

> utt4 = PredVP (DetCN Every (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Woman)))))) (UseV Sleep)

> utt5 = PredVP (DetCN A (RelCN (UseN Man) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))) 
>                  (ComplV2 Love 
>                   (DetCN A (RelCN (UseN Woman) (RelVP (ComplV2 Love (DetCN A (UseN Shake)))))))

> utt6 = PredVP (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))) (UseV Sleep)

> utt7 = PredVP (DetCN Every (ComplN2 Owner (DetCN A (UseN Shake)))) 
>               (ComplV2 Love (DetCN A (UseN Woman)))

> test = mapM_ print . {- nub . -} retrieve . iS

%endif

\bibliographystyle{plainnat}
\bibliography{bringert-bibliography.bib}

\end{document}