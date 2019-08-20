module Fpchat where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

f :: Parser [String]
f = do
  dashes <- many (try (manyTill (char '-') (char '!')))
  return $ filter (\a -> a /= "") (countDashes <$> dashes)

countDashes :: String -> String
countDashes str =
  str ++ if length str > 0 then show (length str) else ""
