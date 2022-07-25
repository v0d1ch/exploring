{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TwoReaders where

import Control.Carrier.Reader
import Control.Carrier.Lift
import qualified Control.Monad.Reader as R
import Control.Monad.IO.Class
import Control.Lens

data EnvOne =
  EnvOne
    { _envOneInt :: Int
    , _envOneString :: String
    } deriving (Eq, Show)

makeClassy ''EnvOne

data EnvTwo =
  EnvTwo
    { _envTwoInt :: Int
    , _envTwoString :: String
    } deriving (Eq, Show)

makeClassy ''EnvTwo

testClassy:: (HasEnvOne r, HasEnvTwo r, R.MonadReader r m, MonadIO m) => m ()
testClassy = do
  e <- R.ask
  liftIO $ print $ e ^. envOneString
  return ()

testFused :: (Has (Reader EnvOne) sig m, Has (Reader EnvTwo) sig m, MonadIO m) => m ()
testFused = do
  EnvOne {..} <- ask @EnvOne
  EnvTwo {..} <- ask @EnvTwo
  liftIO $ print _envOneString
  liftIO $ print _envTwoString
  return ()

mainOne :: IO ()
mainOne =
  runM
    . runReader (EnvOne 1 "a")
    . runReader (EnvTwo 2 "b")
    $ testFused

