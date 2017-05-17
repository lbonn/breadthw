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
suite = [ bench "breadth-first fold" (
            whnf (runIdentity .  foldrT (\_ y -> y + 1 :: Int) 0) (force . fromTTree $ fixedRandomTree 10000)
          )

        , bench "regular tree fold" (
            whnf (foldr' (\_ y -> y + 1 :: Int) 0) (force $ fixedRandomTree 10000)
          )
        ]

main :: IO ()
main = defaultMain suite
