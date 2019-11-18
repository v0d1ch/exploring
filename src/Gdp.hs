{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeOperators          #-}

module Gdp where

import           Data.Coerce

-- newtype Named name a = Named a
-- type a ~~ name = Named name a
-- Morally, the type of `name` is
-- a -> (exists name. (a ~~ name))
-- name :: a -> (forall name . (a ~~ name) -> t) -> t
-- name x k = k (coerce x)

-- class The d a | d -> a where
--   the :: d -> a
--   default the :: Coercible d a => d -> a
--   the = coerce

-- newtype Full a b = Full b

-- instance The (Full a b) b where

newtype Named n a = Named { forgetName :: a } deriving Show

name :: a -> ( forall name. Named name a -> r ) -> r
name x f = f ( Named x )

data IsPrime name = IsPrime deriving Show

checkPrime :: Named name Int -> Maybe (IsPrime name)
checkPrime named | isPrime (forgetName named) = Just IsPrime
                 | otherwise                  = Nothing

isPrime :: Int -> Bool
isPrime 1 = True 
isPrime _ = False
