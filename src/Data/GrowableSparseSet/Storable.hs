module Data.SparseSet.Storable
  ( SparseSetStorable,
    create,
    insert,
    unsafeContains,
    lookup,
    unsafeLookup,
    size,
    remove,
    iterate,
    visualize,
    growDenseIfNecessary,
  )
where

import Control.DeepSeq
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    atomicModifyIORef',
    modifyIORef',
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.IntMap qualified as M
import Data.IntMap.Strict qualified as IM
import Data.Kind (Constraint)
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word (Word32)
import Debug.Trace
import GHC.Generics
import Prelude hiding (iterate, lookup)
import Foreign (Storable(..), peekArray, alloca)
import Foreign.Ptr
import Foreign.C

-- | The sparse set contains a sparse array and a dense array. The 'a' values are stored
-- within the dense array and can be iterated over quickly. The sparse array holds
-- the index of an element to the dense array.
-- The sparse set is useful when you have a lot of possible keys but not that many values
-- to actually store. Iteration over the values is very quick.
data SparseSetStorable a = SparseSetStorable
  { sparseSetSparse :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetEntities :: {-# UNPACK #-} !(VPM.IOVector Word32),
    sparseSetDense :: {-# UNPACK #-} !(VM.IOVector a),
    sparseSetBackup :: {-# UNPACK #-} !(IORef (IM.IntMap a)),
    sparseSetSize :: {-# UNPACK #-} !(IORef Int)
  }
  deriving (Generic)

instance NFData (SparseSetStorable a) where
  rnf (SparseSetStorable {}) = ()

type ElementConstraint a = VM.Storable a :: Constraint

-- | Creates a sparse set with the first value as the sparse array size and the second as the dense array size.
-- Given that the sparse array size is x, then keys from 0..x can be used. maxBound may never be used for x.
-- Given that the dense array size is y, then y values can be stored. y should not be larger than x.
create :: (ElementConstraint a, MonadIO m) => Word32 -> Word32 -> m (SparseSetStorable a)
create sparseSize denseSize = liftIO $ do
  !sparse <- VPM.replicate (fromIntegral sparseSize) maxBound
  !dense <- VM.new (fromIntegral denseSize)
  !entities <- VPM.new (fromIntegral denseSize)
  let !size = 0
  SparseSetStorable sparse entities dense <$> newIORef IM.empty <*> newIORef size
{-# INLINE create #-}

-- | Inserts a value into the sparse set at the given 'Word32' index.
-- Overwrites the old value if there is one.
insert :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> a -> m ()
insert (SparseSetStorable sparse entities dense backupRef sizeRef) i a = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if notFreeOrInBackup index
    then VM.unsafeWrite dense (fromIntegral index) a
    else do
      -- nextIndex <- atomicModifyIORef' sizeRef (\size -> (succ size, size))
      nextIndex <- readIORef sizeRef
      let denseSize = VM.length dense
      if nextIndex < denseSize && index /= inBackup
        then do
          VM.unsafeWrite dense nextIndex a
          VPM.unsafeWrite entities nextIndex i
          VPM.unsafeWrite sparse (fromIntegral i) (fromIntegral nextIndex)
          writeIORef sizeRef (succ nextIndex)
        else do
          VPM.unsafeWrite sparse (fromIntegral i) inBackup
          modifyIORef' backupRef (IM.insert (fromIntegral i) a)
{-# INLINE insert #-}

-- | Returns true if the given key is in the set.
-- | The key must be smaller than the size of the sparse array
unsafeContains :: (MonadIO m) => SparseSetStorable a -> Word32 -> m Bool
unsafeContains (SparseSetStorable sparse _ _ _ _) i = liftIO $ do
  v <- VPM.unsafeRead sparse (fromIntegral i)
  pure $ v /= (free :: Word32)
{-# INLINE unsafeContains #-}

-- | Returns the amount of values in the set
size :: (MonadIO m) => SparseSetStorable a -> m Int
size (SparseSetStorable _ _ _ backupRef sizeRef) =
  liftIO $
    (+) <$> readIORef sizeRef <*> (IM.size <$> readIORef backupRef)
{-# INLINE size #-}

-- | Returns the value at the given index or Nothing if the index is not within the set
lookup :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> m (Maybe a)
lookup (SparseSetStorable sparse _ dense backupRef _) i = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if notFreeOrInBackup index
    then Just <$> VM.unsafeRead dense (fromIntegral index)
    else
      if index == free
        then pure Nothing
        else IM.lookup (fromIntegral i) <$> readIORef backupRef
{-# INLINE lookup #-}

-- | Returns the value at the given index. Only really safe directly after a 'contains' check
--  and may segfault if the index does not exist.
unsafeLookup :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> m a
unsafeLookup (SparseSetStorable sparse _ dense backupRef _) i = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if index /= inBackup
    then VM.unsafeRead dense (fromIntegral index)
    else (IM.! fromIntegral i) <$> readIORef backupRef
{-# INLINE unsafeLookup #-}

-- | Removes an index from the set. Does nothing if the index does not exist.
remove :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> Word32 -> m ()
remove (SparseSetStorable sparse entities dense backupRef sizeRef) i = liftIO $ do
  index <- VPM.unsafeRead sparse (fromIntegral i)
  if notFreeOrInBackup index
    then do
      lastDenseIndex <- atomicModifyIORef sizeRef $ \size -> let s = max 0 (pred size) in (s, s)

      lastElement <- VM.unsafeRead dense lastDenseIndex
      lastKey <- VPM.unsafeRead entities lastDenseIndex

      VM.unsafeWrite dense (fromIntegral index) lastElement
      VPM.unsafeWrite entities (fromIntegral index) lastKey

      VPM.unsafeWrite sparse (fromIntegral lastKey) index
      VPM.unsafeWrite sparse (fromIntegral i) free
    else
      if i == free
        then pure ()
        else do
          VPM.unsafeWrite sparse (fromIntegral i) free
          modifyIORef' backupRef $ IM.delete (fromIntegral i)
{-# INLINE remove #-}

-- | Iterate over all values with their corresponding key.
iterate :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> (Word32 -> a -> m ()) -> m ()
iterate (SparseSetStorable _ entities dense backupRef sizeRef) f = do
  denseSize <- liftIO $ readIORef sizeRef

  forM_ [0 .. pred denseSize] $ \i -> do
    key <- liftIO $ VPM.unsafeRead entities i
    val <- liftIO $ VM.unsafeRead dense i
    f key val

  liftIO (readIORef backupRef)
    >>= IM.foldlWithKey'
      ( \io key val -> do
          io *> f (fromIntegral key) val
      )
      (pure ())
{-# INLINE iterate #-}

-- | Grows the dense array by 50 percent and insert the backed up elements
-- The old sparse set becomes invalid!
growDenseIfNecessary :: (ElementConstraint a, MonadIO m) => SparseSetStorable a -> m (SparseSetStorable a)
growDenseIfNecessary oldSparseSet@(SparseSetStorable sparse entities dense backupRef sizeRef) = liftIO $ do
  backup <- readIORef backupRef
  if IM.size backup == 0
    then pure oldSparseSet
    else do
      let newDenseSize = (VPM.length entities `quot` 2) `max` M.size backup
      newDense <- VM.unsafeGrow dense newDenseSize
      newEntities <- VPM.unsafeGrow entities newDenseSize

      writeIORef backupRef M.empty
      let newSparseSet = SparseSetStorable sparse newEntities newDense backupRef sizeRef

      IM.foldMapWithKey (insert newSparseSet . fromIntegral) backup

      pure newSparseSet

-- | Visualizes the sparse set in the terminal. Mostly for debugging purposes.
visualize :: (MonadIO m) => SparseSetStorable a -> m ()
visualize (SparseSetStorable sparse entities dense backupRef sizeRef) = liftIO $ do
  size <- readIORef sizeRef
  putStrLn $ "SparseSet (" <> show size <> ")"
  putStr "Sparse: "
  VP.freeze sparse >>= print
  putStr "Dense: "
  VP.freeze entities >>= print
  putStr "Backup: "
  readIORef backupRef >>= print . M.keys

inBackup :: Word32
inBackup = maxBound - 1

free :: Word32
free = maxBound

notFreeOrInBackup :: Word32 -> Bool
notFreeOrInBackup = (< maxBound - 1)