import           Protolude

import           Criterion.Main
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

import           Breadthw.ZipTree
import           Tree.Gen

import qualified Data.Tree     as T


fixedRandomTree :: Int -> T.Tree ()
fixedRandomTree s = unG (mkQCGen 1) 1
  where
    (MkGen unG) = resize s simpleTreeGen

suite :: [Benchmark]
suite = [ bench "breadth-first fold (size 1000)" (
            whnf (runIdentity .  foldTree (\_ y -> y + 1 :: Int) 0) (force . fromTTree $ fixedRandomTree 1000)
          )

        , bench "regular tree fold (size 1000)" (
            whnf (foldr' (\_ y -> y + 1 :: Int) 0) (force $ fixedRandomTree 1000)
          )

        , bench "regular tree fold (size 10000)" (
            whnf (foldr' (\_ y -> y + 1 :: Int) 0) (force $ fixedRandomTree 10000)
          )
        ]

main :: IO ()
main = defaultMain suite
