module Main where

import           Protolude
-- import           Test.HUnit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Data.Maybe       (fromJust)

import           Data.Sequence    (fromList)
import qualified Data.Sequence as Seq

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
  [ testCase "Go far left" (
    let z = fromJust $ goPath [1, 1] bigZt in
    assertEqual "far left" 4 $ viewRoot . fromJust $ goDepthFarLeft z 2
  )

  , testCase "Breadth-next root" (
    assertEqual "Breadth next" 2 (viewRoot . fromJust $ breadthNext bigZt)
  )

  , testCase "Breadth-first traversal" (
    let traversal = foldr (\e l -> (root . view) e : l) [] (accum breadthNext bigZt) in
    assertEqual "Breadth traversal" [1,2,3,4,5,6,7,8,9,10,11] traversal
  )
  ]

-- quickcheck

sizesg :: Int -> Gen (Seq Int)
sizesg 0 = return empty
sizesg 1 = return (fromList [1])
sizesg n = do
  p <- choose (1, n-1)
  (p Seq.<|) <$> sizesg (n-p)

simpleTreeGen :: Arbitrary a => Gen (Tree a)
simpleTreeGen = sized . fix $ \f n ->
  if n == 0
     then arbitrary >>= (\e -> return $ Node e empty)
     else do
       r <- sizesg (n-1)
       forest <- mapM f r
       e <- arbitrary
       return $ Node e forest

isAsc :: (Ord a) => [a] -> Bool
isAsc [] = True
isAsc [_] = True
isAsc (x1:x2:xs) = x1 <= x2 && isAsc (x2:xs)

qTests :: TestTree
qTests = testGroup "Quick checks"
  [ testGroup "Breadth-first traversal"
    [ testProperty "Exhaustive" (
        forAll (simpleTreeGen :: Gen (Tree ())) (\t -> length (foldr (:) [] t) == size t)
      )
    , testProperty "Increasing depth" (
        forAll (simpleTreeGen :: Gen (Tree ())) (
          isAsc . map depth . accum breadthNext . fromTree
        )
      )
    ]
  ]


tests :: TestTree
tests =  testGroup "Tests" [ hTests
                           , qTests
                           ]

main :: IO ()
main = defaultMain tests
