module Breadthw.ZipTree where

import           Protolude hiding (orElse)

-- import qualified Data.List     as L
import           Data.Sequence (Seq, (><), (|>), (<|))
import qualified Data.Sequence as Seq


data Tree a = Node a (Forest a) deriving (Show, Eq)
type Forest a = Seq (Tree a)

data Step a = Step a Int (Forest a) (Forest a) deriving (Show, Eq)
type Steps a = [Step a]

type ZipTree a = (Tree a, Steps a)

root :: Tree a -> a
root (Node e _) = e

fromTree :: Tree a -> ZipTree a
fromTree t = (t, [])

view :: ZipTree a -> Tree a
view (t, _) = t

steps :: ZipTree a -> Steps a
steps (_, st) = st

depth :: ZipTree a -> Int
depth (_, st) = length st

idInParent :: ZipTree a -> Maybe Int
idInParent (_, []) = Nothing
idInParent (_, Step _ k _ _ : _) = Just k

farthest :: (a -> Maybe a) -> a -> a
farthest f e =
  case f e of
    Nothing -> e
    Just e1 -> farthest f e1

accum :: (a -> Maybe a) -> a -> [a]
accum f e =
  case f e of
    Nothing -> [e]
    Just e1 -> e : accum f e1

upward :: ZipTree a -> Maybe (ZipTree a)
upward (_, [])                  = Nothing
upward (t, Step r _ f1 f2 : st) = Just (Node r (f1 >< Seq.singleton t >< f2), st)

upToRoot :: ZipTree a -> ZipTree a
upToRoot = farthest upward

goRight :: ZipTree a -> Maybe (ZipTree a)
goRight (t, Step r nc lf rf : st) =
  case Seq.viewl rf of
    Seq.EmptyL     -> Nothing
    (nt Seq.:< xs) -> Just (nt, Step r (nc + 1) (lf |> t) xs : st)

goLeft :: ZipTree a -> Maybe (ZipTree a)
goLeft (t, Step r nc lf rf : st) =
  case Seq.viewr lf of
    Seq.EmptyR     -> Nothing
    (xs Seq.:> nt) -> Just (nt, Step r (nc - 1) xs (t <| rf) : st)

downChild :: Int -> ZipTree a -> Maybe (ZipTree a)
downChild k (Node r forest, steps) =
  if k >= length forest
     then Nothing
     else Just (child, step : steps)
       where
         child = Seq.index forest k
         step = Step r k before (Seq.drop 1 after)
         (before, after) = Seq.splitAt k forest

downChildren :: ZipTree a -> [ZipTree a]
downChildren t = fromMaybe [] $ do
  firstChild <- downChild 0 t
  Just $ accum goRight firstChild

pathFromRoot :: ZipTree a -> [Int]
pathFromRoot (_, steps) = reverse $ map (\(Step _ k _ _) -> k) steps

elemsFromRoot :: ZipTree a -> [a]
elemsFromRoot (_, steps) = reverse $ map (\(Step e _ _ _) -> e) steps

goPath :: [Int] -> ZipTree a -> Maybe (ZipTree a)
goPath [] t = Just t
goPath (x:xs) t = downChild x t >>= goPath xs

foldPath :: (a -> a -> a) -> a -> [Int] -> ZipTree a -> Maybe a
foldPath f c [] t = Just $ f c (root $ view t)
foldPath f c (x:xs) t = do
  child <- downChild x t
  foldPath f (f c (root $ view t)) xs child


-- movements for breadth search

orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just e) _ = Just e
orElse Nothing n = n


-- FIXME: inefficient O(Depth*NChildren), could be O(depth)
childrenRightOfPath :: ZipTree a -> [Int] -> [ZipTree a]
childrenRightOfPath zt p = filter (\c -> p < pathFromRoot c) $ downChildren zt

goDownRightOfPath :: ZipTree a -> [Int] -> Maybe (ZipTree a)
goDownRightOfPath zt p = head $ childrenRightOfPath zt p


goAbsRight :: ZipTree a -> Maybe (ZipTree a)
goAbsRight zt = expl zt
  where
    startPath = pathFromRoot zt
    tdepth = depth zt
    --
    expl :: ZipTree a -> Maybe (ZipTree a)
    expl z = if depth z == tdepth && pathFromRoot z > startPath
                then Just z
                else do
                  next <- goDownRightOfPath z startPath `orElse` upward z
                  expl next

goDepthFarLeft :: ZipTree a -> Int -> Maybe (ZipTree a)
goDepthFarLeft zt d = goFarLeft zt
  where
    goFarLeft z =
      if depth z == d
         then Just z
         else foldr (orElse . goFarLeft) Nothing $ downChildren z

breadthNext :: ZipTree a -> Maybe (ZipTree a)
breadthNext zt =
  goAbsRight zt `orElse` goDepthFarLeft zt (d + 1)
    where
      d = depth zt
