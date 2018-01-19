-- | The Task class that defines the set of combinators to work with Task monad.
--
--   The operations for MonadTask are similar to those of co-routines, with the
--   addition of watching and signaling events.
--
--   We also define a set of auto lifting for common transformers. Note that we
--   purposely leave a case undefined where a state transformer goes on top of
--   a task monad, because such an operation is either unsound or has to roll
--   back the state (see @'Control.Monad.Trans.State.liftCallCC'@). So it's
--   recommended to keep TaskT on top of all StateT in a transformer stack.

{-# LANGUAGE UndecidableInstances,
             FunctionalDependencies,
             MultiParamTypeClasses,
             FlexibleInstances #-}

module Control.Monad.Task.Class
  ( -- * MonadTask class
    MonadTask(..)
  , orElse
  ) where

import Data.Monoid
import Control.Monad.Cont
import Control.Monad.Trans.Identity
import Control.Monad.Trans.List
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.Writer.Lazy as LazyWriter
import qualified Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans.Except

-- | @MonadTask@ specifies a task monad @m@ over an event type @e@.
class Monad m => MonadTask e m | m -> e where
  -- | @yield@ temporarily suspends current task to let others run.
  yield  :: m ()
  -- | @fork@ spawns a task and runs it immediately until it ends or
  --   suspends before returning to current task.
  fork   :: m a -> m ()
  -- | @watch@ suspends the current task to wait for future events, and will
  --   resume execution when an event triggers its watching function.
  watch  :: (e -> Maybe a) -> m a
  -- | @signal@ broadcasts an event to all other tasks that are watching,
  --   and give those who wake up the priority to run.
  signal :: e -> m ()
  -- | @exit@ ends all tasks and returns immediately.
  exit   :: m ()
  -- | @ret@ ends current task.
  ret    :: m ()

-- | @orElse@ is a helper function for combining two trigger functions
--   disjuctively, favoring the first one.
orElse :: (e -> Maybe a) -> (e -> Maybe b) -> e -> Maybe (Either a b)
orElse f g x = maybe (fmap Right (g x)) (Just . Left) (f x)

instance (Monad m, MonadTask a m) => MonadTask a (ExceptT e m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . runExceptT
  watch  = lift . watch
  signal = lift . signal

instance (Monad m, MonadTask a m) => MonadTask a (IdentityT m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . runIdentityT
  watch  = lift . watch
  signal = lift . signal

instance (Monad m, MonadTask a m) => MonadTask a (ListT m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . runListT
  watch  = lift . watch
  signal = lift . signal

instance (Monad m, MonadTask a m) => MonadTask a (MaybeT m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . runMaybeT
  watch  = lift . watch
  signal = lift . signal

instance (Monad m, MonadTask a m) => MonadTask a (ReaderT r m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = ReaderT . (fork .) . runReaderT
  watch  = lift . watch
  signal = lift . signal

instance (Monoid w, Monad m, MonadTask a m) => MonadTask a (LazyWriter.WriterT w m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . LazyWriter.runWriterT
  watch  = lift . watch
  signal = lift . signal

instance (Monoid w, Monad m, MonadTask a m) => MonadTask a (StrictWriter.WriterT w m) where
  ret    = lift ret
  exit   = lift exit
  yield  = lift yield
  fork   = lift . fork . StrictWriter.runWriterT
  watch  = lift . watch
  signal = lift . signal

