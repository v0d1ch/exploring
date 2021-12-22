{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTSyntax                #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Prototype where

import           Control.Concurrent.STM (STM (..), TChan (..), newTChan)
import           Control.Monad.ST

data Process c where
  Stop    :: Process c
  Send    :: c a -> a -> Process c -> Process c
  Receive :: c a -> (a -> Process c) -> Process c
  NewChan :: (c a -> Process c) -> Process c
  (:|:)   :: Process c -> Process c -> Process c

-- The Haskell code in this document uses some language extensions.
-- {-# LANGUAGE ExistentialQuantification, GADTSyntax, RankNTypes #-}
-- We define a type Process :: (* -> *) -> * of concurrent programs,
-- and call its values processes.
-- The type parameter of Process specifies a type of communication channels,
-- which are unbounded FIFO queues.
-- data Process c where
-- Stop :: Process c
-- Send :: c a -> a -> Process c ? Process c
-- Receive :: c a ? (a ? Process c) ? Process c
-- NewChan :: (c a ? Process c) ? Process c
-- (:|:) :: Process c ? Process c ? Process c
-- The intended semantics of processes is as follows:
-- • Stop does nothing.
-- • Send chan val cont writes val into chan and then continues as cont.
-- • Receive chan cont reads a value val from chan and then continues as cont val.
-- • NewChan cont creates a new channel chan and then continues as cont chan.
-- • process1 :|: process2 performs process1 and process2 concurrently.

-- Task 1. Write a function run that runs a given process according to the semantics described above.
-- Your run function may require that the given process uses a particular channel type.

run :: Process a -> Process a
run p =
  case p of
    Stop -> p
    NewChan f -> f p

-- run (Send chan val cont) = undefined --let x = chan <> val  in _a
