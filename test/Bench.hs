import           Protolude

import           Criterion.Main
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

import           Breadthw.ZipTree
import           ZipTree.Gen

fixedRandomTree :: Int -> Tree ()
fixedRandomTree s = unG (mkQCGen 1) 1
  where
    (MkGen unG) = resize s simpleTreeGen

suite :: [Benchmark]
suite = [ bench "breadth first fold of tree of size 1000" (
            whnf (runIdentity .  foldrT (\_ y -> y + 1 :: Int) 0) (force $ fixedRandomTree 1000)
          )
        ]

main :: IO ()
main = defaultMain suite
