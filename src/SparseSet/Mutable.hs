module SparseSet.Mutable where

import Control.Monad.IO.Class
import Data.IORef

newtype Mutable a = Mutable (IORef a)

withMutable :: MonadIO m => Mutable a -> (a -> m (a, b)) -> m b
withMutable (Mutable ref) f = do
  a <- liftIO $ readIORef ref
  (a', b) <- f a
  liftIO $ writeIORef ref a'
  pure b
{-# INLINE withMutable #-}

readMutable :: MonadIO m => Mutable a -> m a
readMutable (Mutable ref) = liftIO $ readIORef ref
{-# INLINE readMutable #-}

writeMutable :: MonadIO m => Mutable a -> a -> m ()
writeMutable (Mutable ref) = liftIO . writeIORef ref
{-# INLINE writeMutable #-}

asMutable :: MonadIO m => a -> m (Mutable a)
asMutable = fmap Mutable . liftIO . newIORef
{-# INLINE asMutable #-}
