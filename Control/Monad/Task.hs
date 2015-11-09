{- | 

Task monad transformer can help refactor event and callback heavy
programs into monads via co-routines. The idea is loosely
based on /Combining Events And Threads For Scalable Network Services/,
by Peng Li and Steve Zdancewic, in /PLDI/, 2007.
(<http://www.cis.upenn.edu/~stevez/papers/abstracts.html#LZ07>), but
with deterministic and co-operative lightweight threads,
also known as co-routines, so that the base monad can be anything ranging 
from IO to state monads, or your favorite monad transformer stack.

Besides, Task monad transformer also provides a simple mechanism to signal
and watch for events, which allows complex event processing logic to be
expressed as streamlined monadic co-routines.

Task monad transformer is essentially a ContT, or continuation transformer,
defined to extract the control flow of monadic programs with co-operative
multi-threading. After the CPS transformation, the program trace is then
executed with a simple round-robin scheduler.

-}

module Control.Monad.Task
  ( -- * MonadTask class
    MonadTask(..)
    -- * TaskT monad transformer
  , TaskT (..)
    -- * Functions
  , runTask
  , orElse
  ) where

import Control.Monad.Trans.Task
import Control.Monad.Task.Class

