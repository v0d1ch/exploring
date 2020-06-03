{-# LANGUAGE ScopedTypeVariables #-}

module Maps where

import qualified Data.Map.Strict as M
import           Prelude
import Data.Foldable (concat)
import Data.Array

test :: IO ()
test = do
  let arr = listArray (0,2) [1,1,0]
  let val = arr ! 0
  print (arr // [(0,val + 1)])
  -- let m = M.fromList [(1, "a"), (2, "b")]
  -- let m' = M.fromList [(3, "c"), (4, "d")]
  -- -- let list = [m, m']
  -- let x = concat m
  -- print x
  -- return ()
