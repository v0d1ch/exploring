{-# LANGUAGE FlexibleContexts #-}

module Logic where

import           Control.Monad.Logic
import           Control.Monad.State

choices :: MonadPlus m => [a] -> m a
choices = msum . map return

evensLogic :: [Int] -> Logic Int
evensLogic ints = choices ints

test :: MonadState String m => [Int] -> m ()
test ints = do
  x <- runLogicT (choices ints) _ -- (\i l -> l)
  return ()
