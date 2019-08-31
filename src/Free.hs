{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE StandaloneDeriving #-}

module Free where

import Control.Monad
import Control.Monad.Free

data TeletypeF a
  = GetLine (String -> a)
  | PutLine String a
  deriving Functor

instance Show a => Show (TeletypeF a) where
  show (GetLine _) = "GetLine"
  show (PutLine str _) = "PutLine " ++ show str

type Teletype = Free TeletypeF

putStrLnTT :: String -> Teletype ()
putStrLnTT str = liftF $ PutLine str ()

getLineTT :: Teletype String
getLineTT = liftF (GetLine id)

echoTT :: Teletype ()
echoTT = forever $ do
  line <- getLineTT
  putStrLnTT line

interpret :: TeletypeF a -> IO a
interpret tt =
  case tt of
    PutLine str a -> putStrLn str >> return a
    GetLine next -> do
      line <- getLine
      return (next line)
