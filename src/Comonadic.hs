{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module Comonadic where

import Control.Comonad
-- import Control.Comonad.Store
import qualified Data.List.NonEmpty as NE
import Prelude

data Store s a = Store (s -> a) s
instance (Show s, Show a) => Show (Store s a) where
   show (Store f a) = "Store (\\" ++ show a ++  " -> " ++ show (f a) ++  " ) " ++ show a

instance Functor (Store s) where
  fmap f (Store f' s) = Store (fmap f f') s

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s

  extend :: forall a b. (Store s a -> b) -> Store s a -> Store s b
  extend f' st@(Store _ a) = Store (\_ -> f' st) a

  duplicate :: forall a. Store s a -> Store s (Store s a)
  duplicate (Store f a) = Store (\x -> Store f x) a


takeSome :: Int -> NE.NonEmpty Int -> NE.NonEmpty Int
takeSome n input = NE.fromList $ NE.take n input

ix :: Int -> NE.NonEmpty Int -> NE.NonEmpty Int
ix index list = NE.fromList . snd $ NE.splitAt index list

-- neToStore :: NE.NonEmpty Int -> Store Int (Maybe Int)
-- neToStore l =
--   store (\i -> head $ NE.filter (\x -> x == i) l) 0

-- extend (takeSome 2) fromList [1..]
