{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving       #-}

-- | The Haskell code in this document uses some language extensions.
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

module Prototype where

import           Control.Concurrent.STM
import           UnliftIO.Async
import           Control.Concurrent.STM.TQueue
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Data.Kind

data Process c where
  Stop :: Process c
  Send :: c a -> a -> Process c -> Process c
  Receive :: c a -> (a -> Process c) -> Process c
  NewChan :: (c a -> Process c) -> Process c
  (:|:) :: Process c -> Process c -> Process c

-- | Task 1. Write a function run that runs a given process according to the semantics described above.
-- Your run function may require that the given process uses a particular channel type.

run :: Process TQueue -> IO (Process TQueue)
run p =
  case p of
    Stop -> return p
    Send ch v pr -> atomically $ do
      writeTQueue ch v
      return pr
    Receive ch f -> atomically $ do
       a <- readTQueue ch
       return $ f a
    NewChan f -> atomically $ do
      ch <- newTQueue
      return $ f ch
    (:|:) p1 p2 -> do
     ea <- race (run p1) (run p2)
     case ea of
       Left p1' -> return p1'
       Right p2' -> return p2'


{-
Task 2. Imagine a community effort whose goal is to find large square numbers.1
Arbitrary people use their computing power to engage in square number search. Your
role is to run a server that accepts submissions of found square numbers and announces
any new record. To achieve this, implement a function 'squareNumServer'

An application 'squareNumServer submissions announcements' shall yield a server process
that receives submissions and sends announcements via the given channels. A submission
not only contains a square number but also its root, which serves as a proof that the
submitted number is indeed a square number. The server must never announce a number
that is not a square number or that does not beat any previous record.
-}

data SquareNumSubmission =
  SquareNumSubmission
    { squareNum :: Integer
    , root :: Integer
    } deriving (Eq, Show)

squareNumServer :: TQueue SquareNumSubmission -> TQueue Integer -> Process TQueue
squareNumServer submissions announcements = Process $ do
  xs <- readTQueue submissions
  undefined
