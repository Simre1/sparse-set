module Data.SparseKeys.NoComponent
  ( SparseKeysNoComponent,
    create,
    nextKey,
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

-- | The Sparse keys are used to create keys with `nextKey`. The keys can be iterated over with `for`.
-- Keys are created in constant time until the sparse keys set is completely full.
data SparseKeysNoComponent = SparseKeysNoComponent
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int),
    sparseSetHeadAndTail :: {-# UNPACK #-} !(IORef (Int, Int))
  }

-- | Creates Sparse Keys with the given size. 
create :: (MonadIO m) => Word32 -> m SparseKeysNoComponent
create size = liftIO $ do
  !sparse <- VPM.generate (fromIntegral size) (\i -> setBit (fromIntegral (succ i)) 31)
  VPM.write sparse (fromIntegral size - 1) (setBit 0 31)
  !entities <- VPM.new (fromIntegral size)
  let !size = 0
  SparseKeysNoComponent sparse entities <$> newIORef size <*> newIORef (1, 0)
{-# INLINE create #-}

-- | Adds a new key to the set and returns it
nextKey :: (MonadIO m) => SparseKeysNoComponent -> m Word32
nextKey (SparseKeysNoComponent sparse entities sizeRef headAndTailRef) = liftIO $ do
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
{-# INLINE nextKey #-}

-- | Returns true if the given key is in the set.
contains :: MonadIO m => SparseKeysNoComponent -> Word32 -> m Bool
contains (SparseKeysNoComponent sparse _ _ _) i = liftIO $ do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ not $ testBit v 31
{-# INLINE contains #-}

-- | Returns the amount of values in the set
size :: MonadIO m => SparseKeysNoComponent -> m Int
size (SparseKeysNoComponent _ _ sizeRef _) = liftIO $ readIORef sizeRef
{-# INLINE size #-}

-- | Removes an index from the set. Does nothing if the index does not exist.
remove :: (MonadIO m) => SparseKeysNoComponent -> Word32 -> m ()
remove (SparseKeysNoComponent sparse entities sizeRef headAndTailRef) i = liftIO $ do
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
          VPM.unsafeWrite sparse (fromIntegral i) (setBit i 31)
          writeIORef headAndTailRef (fromIntegral i, fromIntegral i)
{-# INLINE remove #-}

-- | Iterate over all values with their corresponding key.
for :: (MonadIO m) => SparseKeysNoComponent -> (Word32 -> m ()) -> m ()
for (SparseKeysNoComponent _ entities sizeRef _) f = do
  size <- liftIO $ readIORef sizeRef

  forM_ [0 .. pred size] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i

    f key
{-# INLINE for #-}

-- | Grows the dense array by 50 percent.
growDense :: (MonadIO m) => SparseKeysNoComponent -> m SparseKeysNoComponent
growDense (SparseKeysNoComponent sparse entities sizeRef headAndTailRef) = liftIO $ do
  let entitySize = VPM.length entities
  newEntities <- VPM.unsafeGrow entities (entitySize `quot` 2)
  pure $ SparseKeysNoComponent sparse newEntities sizeRef headAndTailRef

-- | Visualizes the sparse set in the terminal. Mostly for debugging purposes.
visualize :: MonadIO m => SparseKeysNoComponent -> m ()
visualize (SparseKeysNoComponent sparse entities sizeRef headAndTailRef) = liftIO $ do
  size <- readIORef sizeRef
  (head, tail) <- readIORef headAndTailRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStrLn $ "Head/Tail: " <> show head <> "/" <> show tail
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
