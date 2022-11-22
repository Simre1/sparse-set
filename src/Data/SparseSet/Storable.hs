{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.SparseSet.Boxed where

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Coerce (coerce)
import Data.Kind (Constraint)
import Data.Vector.Primitive qualified as VP
import Data.Vector.Primitive.Mutable qualified as VPM
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Data.Word (Word32)
import Foreign.Marshal (malloc, mallocBytes)
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (lookup)
import Data.Foldable (foldlM)
import Data.Vector.Generic.Mutable (set)
import Data.Void
import GHC.ForeignPtr


-- | The sparse set contains a sparse array and a dense array. The 'a' values are stored
-- within the dense array and can be iterated over quickly. The sparse array holds
-- the index of an element to the dense array.
-- The sparse set is useful when you have a lot of possible keys but not that many values
-- to actually store. Iteration over the values is very quick.
newtype SparseSet a = SparseSet (ForeignPtr ())

-- | Creates a sparse set with the first value as the sparse array size and the second as the dense array size.
-- Given that the sparse array size is x, then keys from 0..x can be used. maxBound may never be used for x.
-- Given that the dense array size is y, then y values can be stored. y should not be larger than x.
create :: forall a m. (Storable a, MonadIO m) => Word32 -> Word32 -> m (SparseSet a)
create sparseSize denseSize = liftIO $ do
  fPtr <- mallocForeignPtrBytes (arrayOffset + fromIntegral sparseSize * keySize + fromIntegral denseSize * keyAndValueSize @a)

  unsafeWithForeignPtr fPtr $ \memory -> do
    poke (castPtr memory) (Size 0)

    let sparseSizePtr = memory `plusPtr` sizeOf (undefined :: Size)

    poke (castPtr sparseSizePtr) (SparseSize sparseSize)

    let denseSizePtr = sparseSizePtr `plusPtr` sizeOf (undefined :: SparseSize)

    poke (castPtr denseSizePtr) (DenseSize denseSize)

    let sparseArray = denseSizePtr `plusPtr` sizeOf (undefined :: DenseSize)
    
    writeMaxBound (castPtr sparseArray) sparseSize

  pure $ SparseSet $ fPtr
  where 
    writeMaxBound sparseArray 0 = pure ()
    writeMaxBound sparseArray n = poke sparseArray maxBound >> writeMaxBound (plusPtr sparseArray keySize) (n - 1)  
{-# INLINE create #-}

-- | Inserts a value into the sparse set at the given 'Word32' index.
-- Overwrites the old value if there is one.
insert :: (Storable a, MonadIO m) => SparseSet a -> Word32 -> a -> m (SparseSet a)
insert set i a = withMemory set $ \memory -> liftIO $ do
  let i' = fromIntegral i
  sparseArray <- getSparseArray memory
  denseArray <- getDenseArray memory
  index <- readSparse memory sparseArray i'
  if index /= maxBound
    then do
      writeDenseValue memory denseArray (fromIntegral index) a
      pure memory
    else do
      nextIndex <- getSize memory
      writeSize memory (succ nextIndex)
      let nextIndex' = fromIntegral nextIndex
      writeDense memory denseArray nextIndex' (i, a)
      writeSparse memory sparseArray i' nextIndex
      pure memory
{-# INLINE insert #-}


-- -- | Returns true if the given key is in the set.
contains :: MonadIO m => SparseSet a -> Word32 -> m Bool
contains set i = {-# SCC contains #-}  liftIO $ do
  sparseArray <- getSparseArray set
  v <- readSparse set sparseArray (fromIntegral i)
  pure $ v /= (maxBound :: Word32)
{-# INLINE contains #-}

-- -- | Returns the amount of values in the set
size :: MonadIO m => SparseSet a -> m Int
size = liftIO . fmap fromIntegral . getSize
{-# INLINE size #-}

-- | Returns the value at the given index or Nothing if the index is not within the set
lookup :: (Storable a, MonadIO m) => SparseSet a -> Word32 -> m (Maybe a)
lookup set i = do
  c <- contains set i
  if c
    then liftIO $ fmap Just $ lookup set i
    else pure Nothing
{-# INLINE lookup #-}

-- -- | Returns the value at the given index. Only really safe directly after a 'contains' check
-- --  and may segfault if the index does not exist.
unsafeLookup :: (Storable a, MonadIO m) => SparseSet a -> Word32 -> m a
unsafeLookup set i = liftIO $ do
  sparseArray <- getSparseArray set
  denseArray <- getDenseArray set

  index <- readSparse set sparseArray (fromIntegral i)
  readDenseValue set denseArray (fromIntegral index)
{-# INLINE unsafeLookup #-}

-- -- | Removes an index from the set. Does nothing if the index does not exist.
remove :: forall a m. (Storable a, MonadIO m) => SparseSet a -> Word32 -> m (SparseSet a)
remove set i = liftIO $ do
  let i' = fromIntegral i
  sparseArray <- getSparseArray set
  index <- readSparse set sparseArray i'
  if index == maxBound
    then pure set
    else do
      s <- getSize set
      denseArray <- getDenseArray set

      let lastDenseIndex = pred (max 1 s)
      writeSize set lastDenseIndex

      (lastKey, lastValue) <- readDense set denseArray (fromIntegral lastDenseIndex)

      writeDense set denseArray (fromIntegral index) (lastKey, lastValue)

      writeSparse set sparseArray (fromIntegral lastKey) index

      writeSparse set sparseArray i' maxBound
      pure set
{-# INLINE remove #-}

-- -- | Iterate over all values with their corresponding key.
for :: (Storable a, MonadIO m) => SparseSet a -> (Word32 -> a -> m ()) -> m ()
for set f = do
  s <- liftIO $ getSize set
  denseArray <- liftIO $ getDenseArray set
  forM_ [0 .. pred (fromIntegral s)] $ \i -> do
    (key, val) <- liftIO $ readDense set denseArray i
    f key val
{-# INLINE for #-}

-- | Grows the dense array by 50 percent.
-- growDense :: (ElementConstraint a, MonadIO m) => SparseSet a -> m (SparseSet a)
-- growDense (SparseSet sparse entities dense sizeRef) = liftIO $ do
--   let entitySize = VPM.length entities
--   newDense <- VM.grow dense (entitySize `quot` 2)
--   newEntities <- VPM.grow entities (entitySize `quot` 2)
--   pure $ SparseSet sparse newEntities newDense sizeRef

-- | Visualizes the sparse set in the terminal. Mostly for debugging purposes.
visualize :: Storable a => MonadIO m => SparseSet a -> m ()
visualize set = liftIO $ do
  s <- getSize set
  SparseSize sparseSize <- getSparseSize set
  DenseSize denseSize <- getDenseSize set
  sparseArray <- getSparseArray set
  denseArray <- getDenseArray set

  putStrLn $ "SparseSet (" <> show s <> ")"

  putStr "Sparse: "

  forM_  [0..(pred (fromIntegral sparseSize))] $ \i -> do
    key <- readSparse set sparseArray i
    putStr $ show key <> ", "

  putStrLn ""

  putStr "Dense: "

  forM_  [0..(pred (fromIntegral denseSize))] $ \i -> do
    key <- readDenseKey set denseArray i
    putStr $ show key <> ", "
  
  putStrLn ""



newtype Size = Size Word32 deriving (Storable)

newtype DenseSize = DenseSize Word32 deriving (Show, Eq, Storable, Ord)

newtype SparseSize = SparseSize Word32 deriving (Show, Eq, Storable, Ord)

newtype SparseArray = SparseArray (Ptr ())

newtype DenseArray a = DenseArray (Ptr ())

newtype SparseSetMemory a = SparseSetMemory (Ptr ())

withMemory :: MonadIO m => SparseSet a -> (SparseSetMemory a -> IO ()) -> IO ()
withMemory (SparseSet memory) f = unsafeWithForeignPtr memory (f . SparseSetMemory)

-- withMemoryAndNew :: MonadIO m => SparseSet a -> (SparseSetMemory a -> IO (Maybe (SparseSetMemory a))) -> IO (ForeignPtr a)
-- withMemoryAndNew (SparseSet memory) f = unsafeWithForeignPtr memory (f . SparseSetMemory)

getSize :: SparseSetMemory a -> IO Size
getSize (SparseSetMemory ptr) = peek (castPtr ptr)
{-# INLINE getSize #-}

writeSize :: SparseSetMemory a -> Word32 -> IO ()
writeSize (SparseSetMemory ptr) w = poke (castPtr ptr) w
{-# INLINE writeSize #-}

getSparseSize :: SparseSetMemory a -> IO SparseSize
getSparseSize (SparseSetMemory memory) = do
  peek $ castPtr $ plusPtr memory $ (sizeOf (undefined :: Size))
{-# INLINE getSparseSize #-}

getDenseSize :: SparseSetMemory a -> IO DenseSize
getDenseSize (SparseSetMemory memory) = do
  peek $ castPtr $ plusPtr memory $ sizeOf (undefined ::Size) + sizeOf (undefined :: SparseSize)
{-# INLINE getDenseSize #-}

getSparseArray :: SparseSetMemory a -> IO SparseArray
getSparseArray (SparseSetMemory sparseSet) = do
  let sparseArray = plusPtr sparseSet arrayOffset
  pure (SparseArray sparseArray)
{-# INLINE getSparseArray #-}

getDenseArray :: SparseSetMemory a -> IO (DenseArray a)
getDenseArray sparseSetMemory@(SparseSetMemory sparseSet) = do
  SparseSize s <- getSparseSize sparseSetMemory
  let denseArray = plusPtr sparseSet (arrayOffset + fromIntegral s * keySize)
  pure $ DenseArray denseArray
{-# INLINE getDenseArray #-}

readSparse :: SparseArray -> Int -> IO Word32
readSparse (SparseArray sparseArray) i = peek $ plusPtr $ sparseArray  (i * keySize)
{-# INLINE readSparse #-}

writeSparse :: SparseArray -> Int -> Word32 -> IO ()
writeSparse (SparseArray sparseArray) i = poke $ plusPtr sparseArray (keySize * i)
{-# INLINE writeSparse #-}

readDense :: forall a. Storable a => DenseArray a -> Int -> IO (Word32, a)
readDense (DenseArray denseArray) i = do
  let elementPtr = denseArray plusPtr $ i * keyAndValueSize @a
  e <- peek $ castPtr elementPtr
  v <- peek $ castPtr (plusPtr elementPtr keySize)
  pure (e, v)
{-# INLINE readDense #-}

readDenseValue :: forall a. Storable a => DenseArray a -> Int -> IO a
readDenseValue (DenseArray denseArray) i = do
  let elementPtr = plusPtr denseArray $ i * keyAndValueSize @a
  v <- peek $ castPtr (plusPtr elementPtr keySize)
  pure v
{-# INLINE readDenseValue #-}

readDenseKey :: forall a. Storable a => DenseArray a -> Int -> IO Word32
readDenseKey (DenseArray denseArray) i = do
  let elementPtr = plusPtr denseArray $ i * keyAndValue @a
  v <- peek $ castPtr elementPtr
  pure v
{-# INLINE readDenseKey #-}

writeDense :: forall a. Storable a => DenseArray a -> Int -> (Word32, a) -> IO ()
writeDense (DenseArray denseArray) i (e, v) = do
  let elementPtr = plusPtr denseArray $ i * keyAndValueSize @a
  poke (castPtr elementPtr) e
  poke (castPtr $ plusKey elementPtr) v
{-# INLINE writeDense #-}

writeDenseValue :: forall a. Storable a =>DenseArray a -> Int -> a -> IO ()
writeDenseValue (DenseArray denseArray) i v = do
  let elementPtr = plusPtr denseArray $ i * keyAndValueSize @a
  poke (castPtr $ plusPtr elementPtr keySize) v
{-# INLINE writeDenseValue #-}

keySize :: Int
keySize = sizeOf (undefined :: Word32)
{-# INLINE keySize #-}

keyAndValueSize :: forall a. Int
keyAndValueSize = sizeOf (undefined :: Word32) + sizeOf (undefined :: a)
{-# INLINE keyAndValueSize #-}

arrayOffset :: Int
arrayOffset = 3 * sizeOf (undefined :: Word32)
{-# INLINE arrayOffset #-}
