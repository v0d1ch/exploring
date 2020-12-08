module Failure where

import qualified Control.Error          as ER
import           Control.Exception      (Exception (..))
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans     (lift)
import           Prelude

newtype MyException = MyException String deriving Show

instance Exception MyException

test :: (MonadIO m, MonadThrow m) => m ()
test = do
  throwM $ MyException "xxxx"
  -- res <- ER.runExceptT $ do
  --   _ <- throwM $ MyException "xxxx"
  --   return ()
  -- liftIO $ print "HERE"
  -- liftIO $
  --   case res of
  --     Left err -> print $ "Error" <> err
  --     Right ok -> print ok

test2 :: (MonadIO m, MonadThrow m) => [Int] -> m (Either String [Int])
test2 ints =
  ER.runExceptT $ do
    _ <- lift test
    return ints
  -- mapM
  --  (\i ->
  --     throwM (MyException "OOOpppss")
  --  ) ints

main :: IO ()
main = do
  result <- liftIO $ test2 [1,2,3]
  print result
