{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Main where

import           Protolude
-- import           Test.HUnit
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck

import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.Tree     as T
import           Data.Maybe       (fromJust)


import           Breadthw.ZipTree

import           Tree.Gen

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

newtype DummyExp a = DummyExp (Identity a)
  deriving (Functor, Applicative, Monad)

instance TreeExpand DummyExp () where
  expChildren zt | depth zt == 0 = return [(), ()]
                 | otherwise     = undefined

runDummyExp :: DummyExp a -> a
runDummyExp (DummyExp e) = runIdentity e

hTests :: TestTree
hTests = testGroup "Unit tests"
  [ testCase "Go far left" $
      let z = fromJust $ runPureMaybeT $ goPath [1, 1] bigZt in
      assertEqual "far left" 4 $ viewRoot . fromJust $ runPureMaybeT $ goDepthFarLeft z 2

  , testCase "Breadth-next root" $
      assertEqual "Breadth next" 2 (viewRoot . fromJust $ runPureMaybeT $ breadthNext bigZt)

  , testCase "Breadth-first traversal" $
      let traversal = foldr (\e l -> (root . view) e : l) [] (runIdentity $ accumT breadthNext bigZt) in
      assertEqual "Breadth traversal" [1..11] traversal

  , testCase "Breadth-first laziness" $
      let start = fromTree (Node () FThunk) in
      let simpleNode = Node () FThunk in
      let nextE = view <$> (runDummyExp . runMaybeT $ breadthNext start) in
      assertEqual "Breadth-first laziness" nextE (Just simpleNode)
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
        forAll (simpleTreeGen :: Gen (T.Tree ())) (\tt -> runIdentity $ do
          let t = fromTTree tt
          s <- size $ fromTree t
          return . counterexample (T.drawTree $ fmap (const "A") tt) $ runIdentity (foldTree (\_ y -> y + 1) 0 t) === s
        )

    , testProperty "Increasing depth" $
        forAll (simpleTreeGen :: Gen (T.Tree ())) $
          isAsc . map depth . (runIdentity . accumT breadthNext) . fromTree . fromTTree
    ]
  ]


tests :: TestTree
tests =  testGroup "Tests" [ hTests
                           , qTests
                           ]

main :: IO ()
main = defaultMain tests
