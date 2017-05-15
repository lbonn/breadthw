module Main where

import           Protolude
-- import           Test.HUnit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Data.Maybe       (fromJust)

import           Data.Sequence    (fromList)

import           Breadthw.ZipTree

-- regular unit tests
leaf :: a -> Tree a
leaf e = Node e empty

bigTree :: Tree Int
bigTree = Node 1 $ fromList [Node 2 (fromList [leaf 4, leaf 5, Node 6 $ fromList [leaf 9, leaf 10]])
                           , Node 3 (fromList [leaf 7, Node 8 $ fromList [leaf 11]])
                            ]
bigZt :: ZipTree Int
bigZt = fromTree bigTree

viewRoot :: ZipTree a -> a
viewRoot = root . view

hTests :: TestTree
hTests = testGroup "Unit tests"
  [ testCase "go far left" (
    let z = fromJust $ goPath [1, 1] bigZt in
    assertEqual "far left" 4 $ viewRoot . fromJust $ goDepthFarLeft z 2
  )

  , testCase "breadth-next root" (
    assertEqual "breadth next" 2 (viewRoot . fromJust $ breadthNext bigZt)
  )

  , testCase "breadth-first traversal" (
    let traversal = foldr (\e l -> (root . view) e : l) [] (accum breadthNext bigZt) in
    assertEqual "breadth traversal" [1,2,3,4,5,6,7,8,9,10,11] traversal
  )
  ]

-- quickcheck

qTests :: TestTree
qTests = testGroup "Quick checks"
  [
  ]


tests :: TestTree
tests =  testGroup "Tests" [ hTests
                           , qTests
                           ]

main :: IO ()
main = defaultMain tests
