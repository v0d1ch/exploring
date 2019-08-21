{-# LANGUAGE NoImplicitPrelude #-}

module Comonadic where

import Control.Comonad
import Data.List.NonEmpty
import Prelude (Int, ($))

takeSome :: Int -> NonEmpty Int -> NonEmpty Int
takeSome n input = fromList $ take n input

-- extend (takeSome 2) fromList [1..]
