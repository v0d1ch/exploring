{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes            #-}

module Comonadic where

import Control.Comonad
import Data.Functor.Foldable
import Data.Functor.Identity
import Control.Comonad.Store
import qualified Data.List.NonEmpty as NE
import Prelude
import Safe (headMay)

-- data StoreT s w a = StoreT (w (s -> a)) s

-- instance (Show s, Show a) => Show (StoreT s w a) where
--    show (StoreT f a) = "StoreT (\\" ++ show a ++  " -> " ++ show (f a) ++  " ) " ++ show a

-- instance (Show s, Show a) => Show (Store s a) where
--    show (Store f a) = "Store (\\" ++ show a ++  " -> " ++ show (f a) ++  " ) " ++ show a

-- data Store s a = Store (s -> a) s

-- instance Functor (Store s) where
--   fmap f (Store f' s) = Store (fmap f f') s

-- instance Comonad (Store s) where
--   extract :: Store s a -> a
--   extract (Store f s) = f s

--   extend :: forall a b. (Store s a -> b) -> Store s a -> Store s b
--   extend f' st@(Store _ a) = Store (\_ -> f' st) a

--   duplicate :: forall a. Store s a -> Store s (Store s a)
--   duplicate (Store f a) = Store (\x -> Store f x) a

-- store :: (s -> a) -> s -> Store s a
-- store f s = Store f s

takeSome :: Int -> NE.NonEmpty Int -> NE.NonEmpty Int
takeSome n input = NE.fromList $ NE.take n input

ix :: Int -> NE.NonEmpty Int -> NE.NonEmpty Int
ix index list = NE.fromList . snd $ NE.splitAt index list

neToStore :: NE.NonEmpty Int -> Store Int Int
neToStore l =
  let indexes = [0 .. NE.length l]
    in
      store (\x -> l NE.!! x) 0

neToStore' :: NE.NonEmpty Int -> Store Int Int
neToStore' (a NE.:| as) =
  StoreT (Identity (\x -> x)) a
