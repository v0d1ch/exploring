{-# LANGUAGE RecordWildCards #-}

module Mtl where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Except
import qualified Control.Error as ER

data SomeEnv =
  SomeEnv
   { envA :: Int
   , envB :: String
   } deriving Show

checkIfIntIs8 :: Int -> Either String String
checkIfIntIs8 8 = Right "This int is valid"
checkIfIntIs8 _ = Left "This int is not 8"

checkIfStringIsValid :: String -> Either String String
checkIfStringIsValid "valid" = Right "This string is valid"
checkIfStringIsValid _       = Left "This string is NOT valid"

functionA :: MonadIO m => Int -> ReaderT SomeEnv (ExceptT String m) (String, String)
functionA i = do
  SomeEnv {..} <- ask
  validInt <- lift $ ER.hoistEither $ checkIfIntIs8 i
  validString <- lift $ ER.hoistEither $ checkIfStringIsValid envB
  return (validInt, validString)

functionB :: MonadIO m => ReaderT SomeEnv m (Either () (String,String))
functionB = do
  env@SomeEnv {..} <- ask
  result <- lift $ runExceptT $ runReaderT (functionA envA) env
  case result of
    Left err -> liftIO $ putStrLn err >> return (Left ())
    Right ok -> return (Right ok)

main :: MonadIO m => m (Either () (String, String))
main = do
 let env = SomeEnv 2 "test"
 runReaderT functionB  env
