module Fpchat where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map.Strict as M

type Parser = Parsec Void String

f :: Parser [String]
f = do
  dashes <- many (try (manyTill (char '-') (char '!')))
  return $ filter (\a -> a /= "") (countDashes <$> dashes)

countDashes :: String -> String
countDashes str =
  str ++ if length str > 0 then show (length str) else ""

myMap :: M.Map (Int, Int) Int
myMap = M.fromList
   [ ((-2,0), 3)
   , ((-1,-2), 1)
   , ((-1,-1), 4)
   , ((-1,2), 2)
   , ((1,1), 1)
   , ((1,2), 3)
   , ((2,-1), 7)]

islands :: [[(Int, Int)]]
islands =
  [ [(-1,-1),(-1,-2),(-2,0)]
  , [(-1,2)],[(1,2),(1,1)]
  , [(2,-1)]
  ]
