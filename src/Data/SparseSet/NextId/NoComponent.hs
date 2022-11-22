module Data.SparseSet.NextId.NoComponent
  ( SparseSetNextIdNoComponent,
    create,
    nextId,
    contains,
    size,
    remove,
    for,
    visualize,
    growDense,
  )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits
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
import Data.Word (Word32)
import Prelude hiding (lookup)

-- | The sparse set contains a sparse array and a dense array. The 'a' values are stored
-- within the dense array and can be iterated over quickly. The sparse array holds
-- the index of an element to the dense array.
-- The sparse set is useful when you have a lot of possible keys but not that many values
-- to actually store. Iteration over the values is very quick.
-- This sparse set keeps track of which keys are used and it is possible to get unused keys in constant time.
data SparseSetNextIdNoComponent = SparseSetNextIdNoComponent
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int),
    sparseSetHeadAndTail :: {-# UNPACK #-} !(IORef (Int, Int))
  }

-- | Creates a sparse set with the first value as the sparse array size and the second as the dense array size.
-- Given that the sparse array size is x, then keys from 0..x are used. maxBound may never be used for x.
-- Given that the dense array size is y, then y values can be stored. y should not be larger than x.
create :: (MonadIO m) => Word32 -> Word32 -> m (SparseSetNextIdNoComponent)
create sparseSize denseSize = liftIO $ do
  !sparse <- VPM.generate (fromIntegral sparseSize) (\i -> setBit (fromIntegral (succ i)) 31)
  VPM.write sparse (fromIntegral sparseSize - 1) (setBit 0 31)
  !entities <- VPM.new (fromIntegral denseSize)
  let !size = 0
  SparseSetNextIdNoComponent sparse entities <$> newIORef size <*> newIORef (1, 0)
{-# INLINE create #-}

-- | Adds a new key to the set and returns it
nextId :: (MonadIO m) => SparseSetNextIdNoComponent -> m Word32
nextId (SparseSetNextIdNoComponent sparse entities sizeRef headAndTailRef) = liftIO $ do
  (head, tail) <- readIORef headAndTailRef

  nextIndex <- atomicModifyIORef' sizeRef (\size -> (succ size, size))
  nextHead <- VPM.read sparse head

  VPM.write sparse tail nextHead

  VPM.unsafeWrite entities nextIndex (fromIntegral head)

  VPM.unsafeWrite sparse head (fromIntegral nextIndex)

  let nextHead' = fromIntegral $ clearBit nextHead 31

  if nextHead' == head
    then writeIORef headAndTailRef (-1, -1)
    else writeIORef headAndTailRef (nextHead', tail)
  pure (fromIntegral head)
{-# INLINE nextId #-}

-- | Returns true if the given key is in the set.
contains :: MonadIO m => SparseSetNextIdNoComponent -> Word32 -> m Bool
contains (SparseSetNextIdNoComponent sparse _ _ _) i = liftIO $ do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ not $ testBit v 31
{-# INLINE contains #-}

-- | Returns the amount of values in the set
size :: MonadIO m => SparseSetNextIdNoComponent -> m Int
size (SparseSetNextIdNoComponent _ _ sizeRef _) = liftIO $ readIORef sizeRef
{-# INLINE size #-}

-- | Removes an index from the set. Does nothing if the index does not exist.
remove :: (MonadIO m) => SparseSetNextIdNoComponent -> Word32 -> m ()
remove (SparseSetNextIdNoComponent sparse entities sizeRef headAndTailRef) i = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if testBit index 31
    then pure ()
    else do
      (head, tail) <- readIORef headAndTailRef
      lastDenseIndex <- atomicModifyIORef sizeRef $ \size -> let s = max 0 (pred size) in (s, s)

      lastKey <- VPM.unsafeRead entities lastDenseIndex
      VPM.unsafeWrite entities (fromIntegral index) lastKey
      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      if head /= -1
        then do
          VPM.unsafeWrite sparse tail (setBit i 31)
          VPM.unsafeWrite sparse (fromIntegral i) (setBit (fromIntegral head) 31)
          writeIORef headAndTailRef (head, fromIntegral i)
        else do
          VPM.unsafeWrite sparse (fromIntegral i) ((setBit i 31))
          writeIORef headAndTailRef (fromIntegral i, fromIntegral i)
{-# INLINE remove #-}

-- | Iterate over all values with their corresponding key.
for :: (MonadIO m) => SparseSetNextIdNoComponent -> (Word32 -> m ()) -> m ()
for (SparseSetNextIdNoComponent _ entities sizeRef _) f = do
  size <- liftIO $ readIORef sizeRef

  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i

    f key
{-# INLINE for #-}

-- | Grows the dense array by 50 percent.
growDense :: (MonadIO m) => SparseSetNextIdNoComponent -> m (SparseSetNextIdNoComponent)
growDense (SparseSetNextIdNoComponent sparse entities sizeRef headAndTailRef) = liftIO $ do
  let entitySize = VPM.length entities
  newEntities <- VPM.unsafeGrow entities (entitySize `quot` 2)
  pure $ SparseSetNextIdNoComponent sparse newEntities sizeRef headAndTailRef

-- | Visualizes the sparse set in the terminal. Mostly for debugging purposes.
visualize :: MonadIO m => SparseSetNextIdNoComponent -> m ()
visualize (SparseSetNextIdNoComponent sparse entities sizeRef headAndTailRef) = liftIO $ do
  size <- readIORef sizeRef
  (head, tail) <- readIORef headAndTailRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStrLn $ "Head/Tail: " <> show head <> "/" <> show tail
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
