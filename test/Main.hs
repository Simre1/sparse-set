module Main (main) where

import Data.SparseSet.NextId.NoComponent qualified as N
import Data.SparseSet.Storable qualified as V
import Data.SparseSet.Storable2 qualified as S

import Test.Tasty hiding (defaultMain)
import Data.Word
import Test.Tasty.Bench
import Test.Tasty.HUnit
import Data.Foldable

import Control.Monad.Trans.State

main :: IO ()
main = do
  bench <- performance


  defaultMain [sparseSet, bench]


performance :: IO TestTree
performance = do
    setS <- S.create @Word32 5000 5000
    foldl' (\action i -> action >>= \set -> S.insert set i i) (pure setS) [1..4000]
    setV <- V.create @Word32 5000 5000
    foldl' (\action i -> action >>= \set -> V.insert set i i) (pure setV) [1..4000]
   
    pure $ testGroup "Performance" [
            bench "Vectors" $ whnfIO $ flip evalStateT setV $ V.for setV $ \e v -> get >>= \set -> V.insert set e (v+1) >>= put,
            bench "Ptr" $ whnfIO $ flip evalStateT setS $ S.for setS $ \e v -> get >>= \set -> S.insert set e (v+1) >>= put
      ]

sparseSet :: TestTree
sparseSet =
  testGroup
    "SparseSet"
    [ testGroup
        "Storable"
        [ testCase "Insert/Lookup/Remove" $ do
            set <- S.create @Int 5 5
            S.size set >>= (@?= 0)
            S.insert set 0 10
            S.insert set 1 11
            S.insert set 2 12
            S.lookup set 0 >>= (@?= Just 10)
            S.lookup set 1 >>= (@?= Just 11)
            S.lookup set 2 >>= (@?= Just 12)
            S.lookup set 3 >>= (@?= Nothing)
            S.contains set 1 >>= (@?= True)
            S.size set >>= (@?= 3)
            S.remove set 1
            S.size set >>= (@?= 2)
            S.lookup set 1 >>= (@?= Nothing)
            S.lookup set 0 >>= (@?= Just 10)
            S.lookup set 2 >>= (@?= Just 12)
            S.remove set 0
            S.remove set 2
            S.size set >>= (@?= 0)
            S.contains set 0 >>= (@?= False)
            S.contains set 2 >>= (@?= False)
            (pure () :: IO ())
        ],
      testGroup
        "NextId"
        [ testCase "NextId/Remove" $ do
            set <- N.create 2 2
            N.size set >>= (@?= 0)
            e <- N.nextId set
            N.size set >>= (@?= 1)
            N.contains set e >>= (@?= True)
            N.remove set e
            N.size set >>= (@?= 0)
            N.contains set e >>= (@?= False)
            e1 <- N.nextId set
            e2 <- N.nextId set
            N.size set >>= (@?= 2)
            N.contains set e1 >>= (@?= True)
            N.contains set e2 >>= (@?= True)
            (not $ e1 == e2) @? "e1 and e2 should not be the same"
            N.remove set e1
            e3 <- N.nextId set
            (e1 == e3) @? "e1 and e3 should be the same"
            N.remove set e2
            N.contains set e3 >>= (@?= True)
            N.contains set e2 >>= (@?= False)
            N.remove set e3
            N.nextId set
            N.nextId set
            pure ()
        ]
    ]
