{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module RecursionSchemes2 where

import Control.Arrow
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.List (splitAt, unfoldr)

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

data ListF' a r =
    ConsF a r
  | NilF
  deriving (Show, Functor)

-- fromList :: [a] -> ListF' a a
-- fromList = ana coalg where
--   coalg []    = NilF
--   coalg (h:t) = ConsF h t

knockbackL :: Ord a => [a] -> [a]
knockbackL = apo coalg . project where
  coalg Nil        = Nil
  coalg (Cons x l) = case project l of
    Nil           -> Cons x (Left l)
    Cons h t
      | x <= h    -> Cons x (Left l)
      | otherwise -> Cons h (Right (Cons x t))

insertionSortL :: Ord a => [a] -> [a]
insertionSortL = cata (knockbackL . embed)

-- listToListF' :: forall a . [a] -> List a
-- listToListF' [] =  Fix NilF
-- listToListF' (x:xs) =  Fix $ (ConsF x (listToListF' xs))

-- listFToList :: forall a . List a -> [a]
-- listFToList = cata alg

-- alg :: ListF' a [a] -> [a]
-- alg NilF = []
-- alg (ConsF x acc) =  x : acc

-- xToList :: List X -> [Int]
-- xToList = cata algX
--   where
--     algX NilF = []
--     algX (ConsF a as)
--      | _one a == 0 = 1000 : as
--      | otherwise = _one a : as

data X = X { _one :: Int, _two :: Char } deriving Show

makeBaseFunctor ''X
-- xList :: [X]
-- xList = [X 1 'a', X 2 'b', X 3 'c', X 4 'd', X 5 'e']

-- defX :: X
-- defX = X 0 'x'

-- foldXList :: [X] -> X
-- foldXList xl = foldl (\a b -> X (_one a + _one b) (_two b)) defX xl

-- cataFold :: [X] -> X
-- cataFold = embed $ cata alg
--   where
--     alg Nil = defX
--     alg (Cons x l) =
--         case project l of
--           Nil -> X (_one x + _one defX) (_two x)
--           (Cons x' l') ->  X (_one x' + _one x) (_two x')



-----------------------------------------
-- fix :: (a -> a) -> a
-- fix f = f (fix f)

-- fac :: (Integer -> Integer) -> Integer -> Integer
-- fac self n = if n == 0 then 1 else n * self (n - 1)

-- fac_proto :: Integer -> Integer
-- fac_proto = fix fac
--------------------------------------------

data TreeF c f = EmptyF | LeafF c | NodeF f f
  deriving (Functor)

deriving instance (Show a, Show b) => Show (TreeF a b)
deriving instance (Eq a, Eq b) => Eq (TreeF a b)

makeBaseFunctor ''TreeF

unflatten :: [a] -> TreeF a [a]
unflatten (  []) = EmptyF
unflatten (x:[]) = LeafF x
unflatten (  xs) = NodeF l r
  where (l,r) = splitAt (length xs `div` 2) xs

flatten :: Ord a => TreeF a [a] -> [a]
flatten EmptyF      = []
flatten (LeafF c)   = [c]
flatten (NodeF l r) = mergeLists l r

mergeLists :: Ord a => [a] -> [a] -> [a]
mergeLists = curry $ unfoldr c where
  c ([], [])     = Nothing
  c ([], y:ys)   = Just (y, ([], ys))
  c (x:xs, [])   = Just (x, (xs, []))
  c (x:xs, y:ys) | x <= y = Just (x, (xs, y:ys))
                 | x > y  = Just (y, (x:xs, ys))
  c ((_:_), (_:_))        = Nothing
