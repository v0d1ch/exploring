{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module RecursionSchemes2 where

import Control.Arrow
import Data.Functor.Foldable

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String  }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

newtype Term f = In { out :: f (Term f) }

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]})---add(10, 10)

bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
bottomUp fn =
  out                   ---1) unpack a `Term a` into an `a (Term a)`
  >>> fmap (bottomUp fn)---2) recurse, with fn, into the subterms
  >>> In                ---3) repack the `a (Term a)` into a `Term a`
  >>> fn                ---4) finally, apply fn to the packed `Term a`

mystery :: Functor f => (f b -> b) -> Term f -> b
mystery fn =
  out                  ---1) unpack the Term
  >>> fmap (mystery fn)---2) recursively apply `fn`
  >>> fn               ---3) apply `fn`
----------------------------------------------------------------------------------------

data ListF' a b = NilF | ConsF a b deriving (Show, Functor, Foldable, Traversable)

type List a = Fix (ListF' a)

listToListF' :: forall a . [a] -> List a
listToListF' [] =  Fix NilF
listToListF' (x:xs) =  Fix $ (ConsF x (listToListF' xs))

listFToList :: forall a . List a -> [a]
listFToList = cata alg

alg :: ListF' a [a] -> [a]
alg NilF = []
alg (ConsF x acc) =  x : acc

data X = X {_one :: Int, _two :: String } deriving Show

xToList :: List X -> [Int]
xToList = cata algX
  where
    algX NilF = []
    algX (ConsF a as)
     | _one a == 0 = 1000 : as
     | otherwise = _one a : as
