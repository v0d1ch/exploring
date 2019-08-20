module Fpchat where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

f :: Parser Text
f = do
  let d = '-'
      h = '!'
  first <- try $ char d <|> char h
  dashes <- manyTill (char d) (char h)

  let lDashes = length dashes
      n = lDashes + (if first == d then 1 else 0)
      nDashes = lDashes + 1

  return $ T.pack
    (replicate nDashes d
     ++ (if n == 0 then "-" else show n))
