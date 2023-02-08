module Main (main) where

import Data.SparseKeys.NoComponent qualified as K
import Data.SparseSet.Storable qualified as S
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain sparseSet

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
        "NextKey"
        [ testCase "NextKey/Remove" $ do
            set <- K.create 2
            K.size set >>= (@?= 0)
            e <- K.nextKey set
            K.size set >>= (@?= 1)
            K.contains set e >>= (@?= True)
            K.remove set e
            K.size set >>= (@?= 0)
            K.contains set e >>= (@?= False)
            e1 <- K.nextKey set
            e2 <- K.nextKey set
            K.size set >>= (@?= 2)
            K.contains set e1 >>= (@?= True)
            K.contains set e2 >>= (@?= True)
            (e1 /= e2) @? "e1 and e2 should not be the same"
            K.remove set e1
            e3 <- K.nextKey set
            (e1 == e3) @? "e1 and e3 should be the same"
            K.remove set e2
            K.contains set e3 >>= (@?= True)
            K.contains set e2 >>= (@?= False)
            K.remove set e3
            K.nextKey set
            K.nextKey set
            pure ()
        ]
    ]
