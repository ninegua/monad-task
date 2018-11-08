{-# LANGUAGE GADTs,
             UndecidableInstances,
             NoMonomorphismRestriction,
             GeneralizedNewtypeDeriving,
             MultiParamTypeClasses,
             FlexibleInstances #-}

module Control.Monad.Trans.Task
  ( -- * Task monad transformer
    TaskT (..)
    -- * Trace of a base monad
  , Trace (..)
  , TraceState (..)
  , StepTrace (..)
  , runTrace
  , stepTrace
    -- * Task functions
  , taskToTrace
  , runTask
  ) where

import Control.Applicative
import Control.Monad.Fail (MonadFail(..))
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Task.Class
import Data.Either (partitionEithers)

-- | A @Trace m e@ represents the control flow of a mult-threaded task monad
--   defined over a base monad @m@ and event type @e@.
data Trace m e where
  EXIT   :: Trace m e
  RET    :: Trace m e
  YIELD  :: m (Trace m e) -> Trace m e
  FORK   :: m (Trace m e) -> m (Trace m e) -> Trace m e
  WATCH  :: (e -> Maybe v) -> (v -> m (Trace m e)) -> Trace m e
  POLL   :: (e -> Maybe v) -> (Maybe v -> m (Trace m e)) -> Trace m e
  SIGNAL :: e -> m (Trace m e) -> Trace m e
  SERVE  :: e -> m (Trace m e) -> Trace m e

-- | @runTrace@ runs a trace to its completion in the base monad with a simple
--   round-robin scheduler.
runTrace :: Monad m => m (Trace m e) -> m ()
runTrace = loop . stepTrace
  where
    loop = traceState (\_ -> return ()) (>>= loop) . nextStep

-- | @TraceState e a@ describes either a trace is stalled (expecting input
-- event of type `e`), or can be continued.
data TraceState m e a where
  Stalled :: (e -> a) -> TraceState m e a
  Continued :: (m (Trace m e) -> a) -> m (Trace m e) -> TraceState m e a

-- @traceState@ applies functions to either brach of TraceState.
traceState :: ((e -> a) -> b) -> (a -> b) -> TraceState m e a -> b
traceState f _ (Stalled x)   = f x
traceState _ g (Continued h x) = g (h x)

-- | @StepTrace@ is used to step through a trace
newtype StepTrace m e = StepTrace { nextStep :: TraceState m e (m (StepTrace m e)) }

-- | @stepTrace@ runs a single step of a trace
stepTrace :: Monad m => m (Trace m e) -> StepTrace m e
stepTrace prog = loop [prog] [] [] []
  where
    loop [] [] ws rs = StepTrace $ Stalled $ \x -> return (loop [] [(x,return RET)] ws rs)
    loop (m:ms) ss ws rs = StepTrace $ Continued (fmap step) m
      where
        step EXIT         = loop [] [] [] []
        step RET          = loop ms ss ws rs
        step (YIELD t)    = loop (ms ++ [t]) ss ws rs
        step (FORK t1 t2) = loop (t1:t2:ms) ss ws rs
        step (WATCH f g)  = loop ms ss (ws ++ [WATCH f g]) rs
        step (SIGNAL e t) = loop ms (ss ++ [(e, t)]) ws rs
        step (SERVE e t)  = loop ms ss ws (rs ++ [(e, t)])
        step (POLL f g)   = case pick rs [] of
                              Nothing -> loop (g Nothing : ms) ss ws rs
                              Just (v, t, rs') -> loop (g (Just v) : t : ms) ss ws rs'
          where
            pick [] _ = Nothing
            pick (x@(e,t):xs) ys = case f e of
              Just v  -> Just (v, t, xs ++ reverse ys)
              Nothing -> pick xs (x:ys)
    loop [] ((e,t):ss) ws rs = loop (ms' ++ [t]) ss ws' rs
      where (ms', ws') = partitionEithers evs
            evs = [ maybe (Right x) (Left . g) (f e) | x@(WATCH f g) <- ws ]


-- | Task monad transformer.
newtype TaskT e m a
  = TaskT { runTaskT :: ContT (Trace m e) m a }
  deriving (Functor, Applicative, MonadIO)

-- | @tasktoTrace@ CPS-converts a task monad into a trace in its base monad.
taskToTrace :: Monad m => TaskT e m a -> m (Trace m e)
taskToTrace (TaskT (ContT f)) = f (\_ -> return RET)

-- | @runTask@ runs a task monad until to its completion, i.e., no more active
--   tasks to run, or until it exits.
--
-- * @'runTask' = 'runTrace' . 'taskToTrace'@
runTask :: Monad m => TaskT e m a -> m ()
runTask = runTrace . taskToTrace

instance Monad m => Monad (TaskT e m) where
  return = TaskT . return
  (>>=) m f = TaskT $ runTaskT m >>= runTaskT . f
  fail _ = TaskT $ ContT $ \_ -> return EXIT

instance MonadFail m => MonadFail (TaskT e m) where
  fail = TaskT . Control.Monad.Fail.fail

instance MonadTrans (TaskT e) where
  lift = TaskT . lift

instance MonadReader s m => MonadReader s (TaskT e m) where
  ask = TaskT ask
  local f = TaskT . local f . runTaskT

instance MonadState s m => MonadState s (TaskT e m) where
  get = TaskT get
  put = TaskT . put

instance Monad m => MonadTask e (TaskT e m) where
  ret      = TaskT $ ContT $ \_ -> return RET
  exit     = TaskT $ ContT $ \_ -> return EXIT
  yield    = TaskT $ ContT $ return . YIELD . ($())
  fork p   = TaskT $ ContT $ return . FORK (taskToTrace p) . ($())
  watch f  = TaskT $ ContT $ return . WATCH f
  poll f   = TaskT $ ContT $ return . POLL f
  signal e = TaskT $ ContT $ return . SIGNAL e . ($())
  serve e  = TaskT $ ContT $ return . SERVE e . ($())
