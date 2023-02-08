module Data.SparseSet.NoComponent
  ( SparseSetNoComponent,
    create,
    insert,
    contains,
    size,
    remove,
    iterate,
    visualize,
    growDense,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
  )
import Data.Kind (Constraint)
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Word (Word32)
import Prelude hiding (lookup, iterate)

-- | The sparse set contains a sparse array and a dense array. The 'a' values are stored
-- within the dense array and can be iterated over quickly. The sparse array holds
-- the index of an element to the dense array.
-- The sparse set is useful when you have a lot of possible keys but not that many values
-- to actually store. Iteration over the values is very quick.
data SparseSetNoComponent = SparseSetNoComponent
  { sparseSetSparse :: {-# UNPACK #-} VPM.IOVector Word32,
    sparseSetEntities :: {-# UNPACK #-} VPM.IOVector Word32,
    sparseSetSize :: {-# UNPACK #-} IORef Int
  }

-- | Creates a sparse set with the first value as the sparse array size and the second as the dense array size.
-- Given that the sparse array size is x, then keys from 0..x can be used. maxBound may never be used for x.
-- Given that the dense array size is y, then y values can be stored. y should not be larger than x.
create :: (MonadIO m) => Word32 -> Word32 -> m (SparseSetNoComponent)
create sparseSize denseSize = liftIO $ do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !entities <- VPM.new (fromIntegral denseSize)
  let !size = 0
  SparseSetNoComponent sparse entities <$> newIORef size
{-# INLINE create #-}

-- | Inserts a value into the sparse set at the given 'Word32' index.
-- Overwrites the old value if there is one.
insert :: (MonadIO m) => SparseSetNoComponent -> Word32 -> m ()
insert (SparseSetNoComponent sparse entities sizeRef) i = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= maxBound
    then pure ()
    else do
      nextIndex <- atomicModifyIORef' sizeRef (\size -> (succ size, size))
      let denseSize = VPM.length entities
      VPM.unsafeWrite entities nextIndex i
      VPM.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
{-# INLINE insert #-}

-- | Returns true if the given key is in the set.
contains :: MonadIO m => SparseSetNoComponent -> Word32 -> m Bool
contains (SparseSetNoComponent sparse _ _) i = liftIO $ do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

-- | Returns the amount of values in the set
size :: MonadIO m => SparseSetNoComponent -> m Int
size (SparseSetNoComponent _ _ sizeRef) = liftIO $ readIORef sizeRef
{-# INLINE size #-}

-- | Removes an index from the set. Does nothing if the index does not exist.
remove :: (MonadIO m) => SparseSetNoComponent -> Word32 -> m ()
remove (SparseSetNoComponent sparse entities sizeRef) i = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index == maxBound
    then pure ()
    else do
      lastDenseIndex <- atomicModifyIORef sizeRef $ \size -> let s = max 0 (pred size) in (s, s)

      lastKey <- VPM.unsafeRead entities lastDenseIndex

      VPM.unsafeWrite entities (fromIntegral index) lastKey

      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      VPM.unsafeWrite sparse (fromIntegral i) maxBound
{-# INLINE remove #-}

-- | Iterate over all values with their corresponding key.
iterate :: (MonadIO m) => SparseSetNoComponent -> (Word32 -> m ()) -> m ()
iterate (SparseSetNoComponent _ entities sizeRef) f = do
  size <- liftIO $ readIORef sizeRef

  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i

    f key
{-# INLINE iterate #-}

-- | Grows the dense array by 50 percent.
growDense :: (MonadIO m) => SparseSetNoComponent -> m (SparseSetNoComponent)
growDense (SparseSetNoComponent sparse entities sizeRef) = liftIO $ do
  let entitySize = VPM.length entities
  newEntities <- VPM.unsafeGrow entities (entitySize `quot` 2)
  pure $ SparseSetNoComponent sparse newEntities sizeRef

-- | Visualizes the sparse set in the terminal. Mostly for debugging purposes.
visualize :: MonadIO m => SparseSetNoComponent -> m ()
visualize (SparseSetNoComponent sparse entities sizeRef) = liftIO $ do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
