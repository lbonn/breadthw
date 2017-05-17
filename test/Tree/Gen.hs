module Tree.Gen where

import           Protolude

import qualified Data.Tree as T

import Test.QuickCheck

sizesg :: Int -> Gen [Int]
sizesg 0 = return empty
sizesg 1 = return [1]
sizesg n = do
  p <- choose (1, n-1)
  (p :) <$> sizesg (n-p)

simpleTreeGen :: Arbitrary a => Gen (T.Tree a)
simpleTreeGen = sized . fix $ \f n ->
  if n == 0
     then arbitrary >>= (\e -> return $ T.Node e [])
     else do
       r <- sizesg (n-1)
       forest <- mapM f r
       e <- arbitrary
       return $ T.Node e forest
