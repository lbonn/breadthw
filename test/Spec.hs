module Main where

import           Protolude
-- import           Test.HUnit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import qualified Data.Tree     as T
import           Data.Maybe       (fromJust)


import           Breadthw.ZipTree

import           ZipTree.Gen

-- regular unit tests
bigTree :: Tree Int
bigTree = fromTTree $ T.Node 1  [ T.Node 2 [leaf 4, leaf 5, T.Node 6 [leaf 9, leaf 10]]
                                , T.Node 3 [leaf 7, T.Node 8 [leaf 11]]
                                ]
  where
    leaf e = T.Node e []

bigZt :: ZipTree Int
bigZt = fromTree bigTree

viewRoot :: ZipTree a -> a
viewRoot = root . view

hTests :: TestTree
hTests = testGroup "Unit tests"
  [ testCase "Go far left" $
      let z = fromJust $ runPureMaybeT $ goPath [1, 1] bigZt in
      assertEqual "far left" 4 $ viewRoot . fromJust $ runPureMaybeT $ goDepthFarLeft z 2

  , testCase "Breadth-next root" $
      assertEqual "Breadth next" 2 (viewRoot . fromJust $ runPureMaybeT $ breadthNext bigZt)

  , testCase "Breadth-first traversal" $
      let traversal = foldr (\e l -> (root . view) e : l) [] (runIdentity $ accumT breadthNext bigZt) in
      assertEqual "Breadth traversal" [1,2,3,4,5,6,7,8,9,10,11] traversal
  ]

-- quickcheck

isAsc :: (Ord a) => [a] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x1:x2:xs) = x1 <= x2 && isAsc (x2:xs)

qTests :: TestTree
qTests = testGroup "Quick checks"
  [ testGroup "Breadth-first traversal"
    [ testProperty "Exhaustive" $
        forAll (simpleTreeGen :: Gen (Tree ())) (\t -> runIdentity $ do
          s <- size $ fromTree t
          return $ runIdentity (foldrT (\_ y -> y + 1) 0 t) == s
        )

    , testProperty "Increasing depth" $
        forAll (simpleTreeGen :: Gen (Tree ())) $
          isAsc . map depth . (runIdentity . accumT breadthNext) . fromTree
    ]
  ]


tests :: TestTree
tests =  testGroup "Tests" [ hTests
                           , qTests
                           ]

main :: IO ()
main = defaultMain tests
