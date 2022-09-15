{-# Language PatternSynonyms #-}

module SparseSet.Storable
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Kind (Constraint)
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word (Word32)
import SparseSet.Mutable
import Prelude hiding (lookup)
import Data.Mutable

-- | The sparse set contains a sparse array and a dense array. The 'a' values are stored
-- within the dense array and can be iterated over quickly. The sparse array holds
-- the index of an element to the dense array.
-- The sparse set is useful when you have a lot of possible keys but not that many values
-- to actually store. Iteration over the values is very quick.

data SparseSetStorable a = SparseSetStorable
  { 
    sparseSetSize :: {-# UNPACK #-} !(IORef Int),
    sparseSetSets :: {-# UNPACK #-} !(IORef (Sets a))
}

data Sets a = Sets {
    sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetDense :: {-# UNPACK #-} !(VM.IOVector a)
}

type ElementConstraint a = V.Storable a :: Constraint

-- | Creates a sparse set with the first value as the sparse array size and the second as the dense array size.
-- Given that the sparse array size is x, then keys from 0..x can be used. maxBound may never be used for x.
-- Given that the dense array size is y, then y values can be stored. y should not be larger than x.
create :: forall a m. (ElementConstraint a, MonadIO m) => Word32 -> Word32 -> m (SparseSetStorable a)
create sparseSize denseSize = liftIO $ do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !dense <- VM.new @_ @a (fromIntegral denseSize)
  !entities <- VPM.new (fromIntegral denseSize)
  sizeRef <- newIORef 0
  sets <- newIORef $ Sets sparse entities dense
  pure $ SparseSetStorable sizeRef sets
{-# INLINE create #-}

-- | Inserts a value into the sparse set at the given 'Word32' index.
-- Overwrites the old value if there is one.
insert :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> a -> m ()
insert (SparseSetStorable sizeRef setsRef) i a = liftIO $ do
  (Sets sparse entities dense) <- liftIO $ readIORef setsRef

  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then do
      VM.unsafeWrite dense (fromIntegral index) a
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\s -> (succ s, s))
      let denseSize = VM.length dense
      VM.unsafeWrite dense nextIndex a
      VPM.unsafeWrite entities nextIndex i
      VPM.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

-- | Returns true if the given key is in the set.
contains :: MonadIO m => SparseSetStorable a -> Word32 -> m Bool
contains (SparseSetStorable _ setsRef) i = liftIO $ do
  (Sets sparse _ _) <- readIORef setsRef
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

-- | Returns the amount of values in the set
size :: MonadIO m => SparseSetStorable a -> m Int
size (SparseSetStorable sizeRef setsRef) = liftIO $ readIORef sizeRef
{-# INLINE size #-}

-- | Returns the value at the given index or Nothing if the index is not within the set
lookup :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> m (Maybe a)
lookup (SparseSetStorable _ sets) i = liftIO $ do
  (Sets sparse _ dense) <- readIORef sets
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then Just <$> VM.unsafeRead dense (fromIntegral index)
    else pure Nothing
{-# INLINE lookup #-}

-- | Returns the value at the given index. Only really safe directly after a 'contains' check
--  and may segfault if the index does not exist.
unsafeLookup :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> m a
unsafeLookup (SparseSetStorable _ sets) i = liftIO $ do
  (Sets sparse _ dense) <- readIORef sets
  index <- VPM.unsafeRead sparse (fromIntegral i)
  VM.unsafeRead dense (fromIntegral index)
{-# INLINE unsafeLookup #-}

-- | Removes an index from the set. Does nothing if the index does not exist.
remove :: forall a m. (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> m ()
remove (SparseSetStorable sizeRef sets) i = liftIO $ do
  sets@(Sets sparse entities dense) <- readIORef sets
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do

      lastDenseIndex <- atomicModifyIORef' sizeRef $ \size -> let x = max 0 size in (x,x)

      lastElement <- VM.unsafeRead @_ @a dense lastDenseIndex
      lastKey <- VPM.unsafeRead entities lastDenseIndex

      VM.unsafeWrite dense (fromIntegral index) lastElement
      VPM.unsafeWrite entities (fromIntegral index) lastKey

      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      VPM.unsafeWrite sparse (fromIntegral i) maxBound
      pure ()
{-# INLINE remove #-}

-- | Iterate over all values with their corresponding key.
for :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> (Word32 -> a -> m ()) -> m ()
for (SparseSetStorable sizeRef sets) f = do
  size <- liftIO $ readIORef sizeRef
  (Sets _ entities dense) <- liftIO $ readIORef sets
  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i
    val <- liftIO $ VM.unsafeRead dense i
    f key val
{-# INLINE for #-}

-- | Grows the dense array by 50 percent.
-- growDense :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> m (SparseSetStorable a)
-- growDense (SparseSetStorable sparse entities dense sizeRef) = liftIO $ do
--   let entitySize = VPM.length entities
--   newDense <- VM.unsafeGrow dense (entitySize `quot` 2)
--   newEntities <- VPM.unsafeGrow entities (entitySize `quot` 2)
--   pure $ SparseSetStorable sparse newEntities newDense sizeRef

-- | Visualizes the sparse set in the terminal. Mostly for debugging purposes.
visualize :: MonadIO m => SparseSetStorable a -> m ()
visualize = undefined 
-- (SparseSetStorable sizeRef sparse entities dense) = liftIO $ do
--   size <- readIORef sizeRef
--   putStrLn $ "SparseSet (" <> show size <> ")"
--   putStr "Sparse: "
--   VP.freeze sparse >>= print
--   putStr "Dense: "
--   VP.freeze entities >>= print
