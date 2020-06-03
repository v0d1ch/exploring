module Main where

import           BeautifulFolds
import           Prelude

main :: IO ()
main = do
  -- let x = sum [1..1000000000]
  let x = foldFast sumFast [1..1000000000]
  print x
