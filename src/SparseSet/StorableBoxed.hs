{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module SparseSet.StorableBoxed where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.IORef
  ( IORef,
    atomicModifyIORef,
    atomicModifyIORef',
    newIORef,
    writeIORef,
  )
import Data.Kind (Constraint)
import Data.Mutable
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word (Word32)
import Foreign.Marshal (malloc, mallocBytes)
import Foreign.Ptr
import Foreign.Storable
import SparseSet.Mutable
import Prelude hiding (lookup)
import Data.Foldable (foldlM)
import Data.Vector.Generic.Mutable (set)
import Foreign.StablePtr

-- | The sparse set contains a sparse array and a dense array. The 'a' values are stored
-- within the dense array and can be iterated over quickly. The sparse array holds
-- the index of an element to the dense array.
-- The sparse set is useful when you have a lot of possible keys but not that many values
-- to actually store. Iteration over the values is very quick.

-- Size
-- SparseSize
-- DenseSize
-- Sparse
-- Dense

newtype SparseArray = SparseArray (Ptr ())

newtype DenseArray a = DenseArray (Ptr ())

sizeOf' :: forall a. Storable a => Int
sizeOf' = sizeOf (undefined :: a)

newtype Size = Size Word32 deriving (Storable)

newtype SparseSize = SparseSize Word32 deriving (Storable)

newtype DenseSize = DenseSize Word32 deriving (Storable)

newtype SparseSetStorable2 a = SparseSetStorable2 (Ptr (SparseSetStorable a))

newtype SparseSetStorable (a :: *) = SparseSetStorable (Ptr ()) deriving Storable

arrayOffset :: Int
arrayOffset = 3 * sizeOf' @Word32
{-# INLINE arrayOffset #-}

getSparseArray :: SparseSetStorable a -> IO SparseArray
getSparseArray (SparseSetStorable ptr) = do
  let sparseArray = ptr `plusPtr` arrayOffset
  pure (SparseArray sparseArray)
{-# INLINE getSparseArray #-}

unsafeReadSparse :: SparseSetStorable a -> SparseArray -> Int -> IO Word32
unsafeReadSparse _ (SparseArray ptr) i = peek $ ptr `plusPtr` (i * sizeOf' @Word32)
{-# INLINE unsafeReadSparse #-}

unsafeWriteSparse :: SparseSetStorable a -> SparseArray -> Int -> Word32 -> IO ()
unsafeWriteSparse _ (SparseArray ptr) i = poke $ ptr `plusPtr` (i * sizeOf' @Word32)
{-# INLINE unsafeWriteSparse #-}

getDenseArray :: SparseSetStorable a -> IO (DenseArray a)
getDenseArray (SparseSetStorable ptr) = do
  let sparseSizePtr = castPtr (ptr `plusPtr` (sizeOf' @Size))
  SparseSize s <- peek sparseSizePtr
  let denseArray = ptr `plusPtr` (arrayOffset + fromIntegral s * sizeOf' @Word32)
  pure $ DenseArray denseArray
{-# INLINE getDenseArray #-}

unsafeReadDense :: forall a.SparseSetStorable a -> DenseArray a -> Int -> IO (Word32, a)
unsafeReadDense _ (DenseArray ptr) i = do
  let elementPtr = ptr `plusPtr` (i * (sizeOf' @Word32 + sizeOf' @(StablePtr a)))
  e <- peek $ castPtr elementPtr
  p_v <- peek $ castPtr (elementPtr `plusPtr` sizeOf' @Word32)
  v <- deRefStablePtr p_v
  pure (e, v)
{-# INLINE unsafeReadDense #-}

unsafeReadDenseValue :: forall a. SparseSetStorable a -> DenseArray a -> Int -> IO a
unsafeReadDenseValue _ (DenseArray ptr) i = do
  let elementPtr = ptr `plusPtr` (i * (sizeOf' @Word32 + sizeOf' @(StablePtr a)))
  p_v <- peek $ castPtr (elementPtr `plusPtr` sizeOf' @Word32)
  v <- deRefStablePtr p_v
  pure v
{-# INLINE unsafeReadDenseValue #-}


unsafeReadDenseKey :: forall (a :: *). SparseSetStorable a -> DenseArray a -> Int -> IO Word32
unsafeReadDenseKey _ (DenseArray ptr) i = do
  let elementPtr = ptr `plusPtr` (i * (sizeOf' @Word32 + sizeOf' @(StablePtr a)))
  k <- peek $ castPtr elementPtr
  pure k
{-# INLINE unsafeReadDenseKey #-}

unsafeWriteDense :: forall a. SparseSetStorable a -> DenseArray a -> Int -> (Word32, a) -> IO ()
unsafeWriteDense _ (DenseArray ptr) i (e, v) = do
  let elementPtr = ptr `plusPtr` (i * (sizeOf' @Word32 + sizeOf' @(StablePtr a)))
  old_ptr :: StablePtr a <- peek (castPtr (elementPtr `plusPtr` sizeOf' @Word32)) 
  poke (castPtr elementPtr) e
  freeStablePtr old_ptr
  ptr <- newStablePtr v
  poke (castPtr (elementPtr `plusPtr` sizeOf' @Word32)) ptr
{-# INLINE unsafeWriteDense #-}

unsafeWriteDenseValue :: forall a. SparseSetStorable a -> DenseArray a -> Int -> a -> IO ()
unsafeWriteDenseValue _ (DenseArray ptr) i v = do
  let elementPtr = ptr `plusPtr` (i * (sizeOf' @Word32 + sizeOf' @(StablePtr a)))
  ptr <- newStablePtr v
  poke (castPtr (elementPtr `plusPtr` sizeOf' @Word32)) ptr
{-# INLINE unsafeWriteDenseValue #-}

getSize :: SparseSetStorable a -> IO Word32
getSize (SparseSetStorable ptr) = peek (castPtr ptr)
{-# INLINE getSize #-}

getDenseSize :: SparseSetStorable a -> IO DenseSize
getDenseSize (SparseSetStorable ptr) = do
  peek (castPtr $ ptr `plusPtr` (sizeOf' @Size + sizeOf' @SparseSize))
{-# INLINE getDenseSize #-}

getSparseSize :: SparseSetStorable a -> IO SparseSize
getSparseSize (SparseSetStorable ptr) = do
  peek (castPtr $ ptr `plusPtr` (sizeOf' @Size))
{-# INLINE getSparseSize #-}

writeSize :: SparseSetStorable a -> Word32 -> IO ()
writeSize (SparseSetStorable ptr) w = poke (castPtr ptr) w
{-# INLINE writeSize #-}

type ElementConstraint a = () :: Constraint

-- | Creates a sparse set with the first value as the sparse array size and the second as the dense array size.
-- Given that the sparse array size is x, then keys from 0..x can be used. maxBound may never be used for x.
-- Given that the dense array size is y, then y values can be stored. y should not be larger than x.
create :: forall (a :: *) m. (ElementConstraint a, MonadIO m) => Word32 -> Word32 -> m (SparseSetStorable2 a)
create sparseSize denseSize = liftIO $ do
  ptr <- mallocBytes (3 * sizeOf' @Word32 + sizeOf' @Word32 * fromIntegral sparseSize + (sizeOf' @Word32 + sizeOf' @(StablePtr a)) * fromIntegral denseSize)

  poke (castPtr ptr) (Size 0)

  let sparseSizePtr = ptr `plusPtr` sizeOf' @Size

  poke (castPtr sparseSizePtr) (SparseSize sparseSize)

  let denseSizePtr = sparseSizePtr `plusPtr` sizeOf' @SparseSize

  poke (castPtr denseSizePtr) (DenseSize denseSize)

  let sparseArray :: Ptr Word32 = castPtr $ denseSizePtr `plusPtr` sizeOf' @DenseSize
  
  writeMaxBound (sparseArray) sparseSize

  wrapper <- malloc

  poke wrapper (SparseSetStorable ptr) 

  pure $ SparseSetStorable2 wrapper

  where 
    writeMaxBound ptr 0 = pure ()
    writeMaxBound ptr n = poke ptr maxBound >> writeMaxBound (ptr `plusPtr` sizeOf' @Word32) (n - 1)  
{-# INLINE create #-}

-- | Inserts a value into the sparse set at the given 'Word32' index.
-- Overwrites the old value if there is one.
insert :: (ElementConstraint a, MonadIO m) => SparseSetStorable2 a -> Word32 -> a -> m ()
insert (SparseSetStorable2 set') i a = {-# SCC insert #-} liftIO $ do
  set <- peek set'
  let i' = fromIntegral i
  sparseArray <- getSparseArray set
  denseArray <- getDenseArray set
  index <- unsafeReadSparse set sparseArray i'
  if index /= maxBound
    then do
      unsafeWriteDenseValue set denseArray (fromIntegral index) a
    else do
      nextIndex <- getSize set
      writeSize set (succ nextIndex)
      let nextIndex' = fromIntegral nextIndex
      unsafeWriteDense set denseArray nextIndex' (i, a)
      unsafeWriteSparse set sparseArray i' nextIndex
{-# INLINE insert #-}


-- -- | Returns true if the given key is in the set.
contains :: MonadIO m => SparseSetStorable2 a -> Word32 -> m Bool
contains set' i = {-# SCC contains #-}  liftIO $ do
  set <- peek (coerce set')
  sparseArray <- getSparseArray set
  v <- unsafeReadSparse set sparseArray (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

-- -- | Returns the amount of values in the set
size :: MonadIO m => SparseSetStorable2 a -> m Int
size (SparseSetStorable2 set') = liftIO $ peek set' >>= fmap fromIntegral . getSize
{-# INLINE size #-}

-- | Returns the value at the given index or Nothing if the index is not within the set
lookup :: (ElementConstraint a, MonadIO m) => SparseSetStorable2 a -> Word32 -> m (Maybe a)
lookup set i = do
  c <- contains set i
  if c
    then liftIO $ fmap Just $ unsafeLookup set i
    else pure Nothing
{-# INLINE lookup #-}

-- -- | Returns the value at the given index. Only really safe directly after a 'contains' check
-- --  and may segfault if the index does not exist.
unsafeLookup :: (ElementConstraint a, MonadIO m) => SparseSetStorable2 a -> Word32 -> m a
unsafeLookup set' i = {-# SCC unsafeLookup #-}  liftIO $ do
  set <- peek (coerce set')
  sparseArray <- getSparseArray set
  denseArray <- getDenseArray set

  index <- unsafeReadSparse set sparseArray (fromIntegral i)
  unsafeReadDenseValue set denseArray (fromIntegral index)
{-# INLINE unsafeLookup #-}

-- -- | Removes an index from the set. Does nothing if the index does not exist.
remove :: forall a m. (ElementConstraint a, MonadIO m) => SparseSetStorable2 a -> Word32 -> m ()
remove set' i = liftIO $ do
  set :: SparseSetStorable a <- peek (coerce set')
  let i' = fromIntegral i
  sparseArray <- getSparseArray set
  index <- unsafeReadSparse set sparseArray i'
  if index == maxBound
    then pure ()
    else do
      s <- getSize set
      denseArray <- getDenseArray set

      let lastDenseIndex = pred (max 1 s)
      writeSize set lastDenseIndex

      (lastKey, lastValue) <- unsafeReadDense set denseArray (fromIntegral lastDenseIndex)

      unsafeWriteDense set denseArray (fromIntegral index) (lastKey, lastValue)

      unsafeWriteSparse set sparseArray (fromIntegral lastKey) index

      unsafeWriteSparse set sparseArray i' maxBound
      pure ()
{-# INLINE remove #-}

-- -- | Iterate over all values with their corresponding key.
for :: (ElementConstraint a, MonadIO m) => SparseSetStorable2 a -> (Word32 -> a -> m ()) -> m ()
for set' f = {-# SCC for #-} do
  set <- liftIO $ peek (coerce set')
  s <- liftIO $ getSize set
  denseArray <- liftIO $ getDenseArray set
  forM_ [0 .. pred (fromIntegral s)] $ \i -> do
    (key, val) <- liftIO $ unsafeReadDense set denseArray i
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
visualize set = liftIO $ do
  s <- getSize set
  SparseSize sparseSize <- getSparseSize set
  DenseSize denseSize <- getDenseSize set
  sparseArray <- getSparseArray set
  denseArray <- getDenseArray set

  putStrLn $ "SparseSet (" <> show s <> ")"

  putStr "Sparse: "

  forM_  [0..(pred (fromIntegral sparseSize))] $ \i -> do
    key <- unsafeReadSparse set sparseArray i
    putStr $ show key <> ", "

  putStrLn ""

  putStr "Dense: "

  forM_  [0..(pred (fromIntegral denseSize))] $ \i -> do
    key <- unsafeReadDenseKey set denseArray i
    putStr $ show key <> ", "
  
  putStrLn ""


