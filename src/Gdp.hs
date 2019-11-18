{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Gdp where

import           Data.Coerce

newtype Named name a = Named a
type a ~~ name = Named name a
-- Morally, the type of `name` is
-- a -> (exists name. (a ~~ name))
name :: a -> (forall name . (a ~~ name) -> t) -> t
name x k = k (coerce x)

class The d a | d -> a where
  the :: d -> a
  default the :: Coercible d a => d -> a
  the = coerce
