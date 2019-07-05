-- {-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE FlexibleContexts #-}

-- module Mtl where

-- import Control.Monad.IO.Class
-- import Control.Monad.Trans.Reader
-- import Control.Monad.Except
-- import Control.Monad.Reader hiding (ask)
-- import qualified Control.Error as ER

-- data SomeEnv =
--   SomeEnv
--    { envA :: Int
--    , envB :: String
--    } deriving Show

-- checkIfIntIs8 :: Int -> Either String String
-- checkIfIntIs8 8 = Right "This int is valid"
-- checkIfIntIs8 _ = Left "This int is not 8"

-- checkIfStringIsValid :: String -> Either String String
-- checkIfStringIsValid "valid" = Right "This string is valid"
-- checkIfStringIsValid _       = Left "This string is NOT valid"

-- functionB :: MonadIO m => Int -> ReaderT SomeEnv (ExceptT String m) (String, String)
-- functionB i = do
--   SomeEnv {..} <- ask
--   validInt <- lift $ ER.hoistEither $ checkIfIntIs8 i
--   validString <- lift $ ER.hoistEither $ checkIfStringIsValid envB
--   return (validInt, validString)

-- functionA :: MonadIO m => ReaderT SomeEnv m (Either () (String,String))
-- functionA = do
--   env@SomeEnv {..} <- ask
--   result <- lift $ runExceptT $ runReaderT (functionB envA) env
--   case result of
--     Left err -> liftIO $ putStrLn err >> return (Left ())
--     Right ok -> return (Right ok)

-- functionA' :: (MonadIO m, MonadReader SomeEnv m, MonadError String m) => m (String,String)
-- functionA' = undefined

-- functionB' :: (MonadIO m, MonadReader SomeEnv m, MonadError String m) => m (String,String)
-- functionB' = undefined

-- main :: MonadIO m => m (Either () (String, String))
-- main = do
--  let env = SomeEnv 2 "test"
--  runReaderT functionA  env

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mtl where

import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader

data SomeEnv =
  SomeEnv
   { envA :: Int
   , envB :: String
   } deriving Show

data MtlException = MtlException String deriving (Show)

instance Exception MtlException

checkIfIntIs8 :: MonadCatch m => Int -> m String
checkIfIntIs8 8 = return     "This int is valid"
checkIfIntIs8 _ = throwM (MtlException "This int is not 8")

checkIfStringIsValid :: MonadCatch m => String -> m String
checkIfStringIsValid "valid" = return     "This string is valid"
checkIfStringIsValid _       = throwM (MtlException "This string is NOT valid")

functionB
  :: ( MonadIO m
     , MonadReader SomeEnv m
     , MonadCatch m
     )
  => Int -> m (String, String)
functionB i = do
  SomeEnv {..} <- ask
  validInt       <- checkIfIntIs8 i
  validString    <- checkIfStringIsValid envB
  return (validInt, validString)

functionA
  :: ( MonadIO m
     , MonadReader SomeEnv m
     , MonadCatch m)
  => m (String,String)
functionA = do
  SomeEnv {..} <- ask
  res <- functionB envA
  return res `catch` \(err :: MtlException) -> throwM err

main :: IO ()
main = do
  let env = SomeEnv 4 "valid"
  res <- runExceptT $ runReaderT functionA env
  case res of
    Left  err -> error err
    Right ok -> print ok
