module ZipTree.Gen where

import           Protolude

import           Data.Sequence    (fromList)
import qualified Data.Sequence as Seq

import           Breadthw.ZipTree

import Test.QuickCheck

sizesg :: Int -> Gen (Seq Int)
sizesg 0 = return empty
sizesg 1 = return (fromList [1])
sizesg n = do
  p <- choose (1, n-1)
  (p Seq.<|) <$> sizesg (n-p)

simpleTreeGen :: Arbitrary a => Gen (Tree a)
simpleTreeGen = sized . fix $ \f n ->
  if n == 0
     then arbitrary >>= (\e -> return $ Node e (FVal empty))
     else do
       r <- sizesg (n-1)
       forest <- mapM f r
       e <- arbitrary
       return $ Node e (FVal forest)
