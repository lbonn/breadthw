{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Breadthw.ZipTree where

import           Protolude hiding (orElse)

-- import qualified Data.List     as L
import qualified Data.Tree     as T
import           Data.Sequence (Seq, (><), (|>), (<|))
import qualified Data.Sequence as Seq

import           Control.Monad.Fail
import           Control.Monad.Trans.Maybe


data Tree a = Node a (Forest a) deriving (Show, Eq)
data Forest a = FThunk | FVal (Seq (Tree a)) deriving (Show, Eq)

instance NFData a => NFData (Tree a) where
  rnf (Node e FThunk)   = rnf e
  rnf (Node e (FVal s)) = rnf e `seq` rnf s

fromTTree :: T.Tree a -> Tree a
fromTTree tt = Node r (FVal $ Seq.fromList . map fromTTree $ f)
  where
    r = T.rootLabel tt
    f = T.subForest tt

size :: (TreeExpand m a) => ZipTree a -> m Int
size zt = do
  sizes <- mapM size =<< downChildren zt
  return $ 1 + sum sizes

data Step a = Step a Int (Forest a) (Forest a) deriving (Show, Eq)
type Steps a = [Step a]

type ZipTree a = (Tree a, Steps a)

class Monad m => TreeExpand m a where
  expChildren :: ZipTree a -> m [a]

instance TreeExpand Identity a where
  expChildren _ = return []

runPureMaybeT :: MaybeT Identity a -> Maybe a
runPureMaybeT = runIdentity . runMaybeT

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

-- | apply a function repeatedly until 'Nothing'
farthest :: (a -> Maybe a) -> a -> a
farthest f e =
  case f e of
    Nothing -> e
    Just e1 -> farthest f e1

accumT :: (Monad m) => (a -> MaybeT m a) -> a -> m [a]
accumT f e = do
  r <- runMaybeT $ f e
  case r of
    Nothing -> return [e]
    Just e1 -> map (e :) $ accumT f e1

accum :: (a -> Maybe a) -> a -> [a]
accum f e = runIdentity $ accumT (MaybeT . return . f) e

upward :: ZipTree a -> Maybe (ZipTree a)
upward (_, []) = Nothing
upward (t, Step r _ (FVal f1) (FVal f2) : st) = Just (Node r (FVal $ f1 >< Seq.singleton t >< f2), st)
upward _ = undefined  -- should not be possible

upToRoot :: ZipTree a -> ZipTree a
upToRoot = farthest upward

goRight :: ZipTree a -> Maybe (ZipTree a)
goRight (_, []) = Nothing
goRight (t, Step r nc (FVal lf) (FVal rf) : st) =
  case Seq.viewl rf of
    Seq.EmptyL     -> Nothing
    (nt Seq.:< xs) -> Just (nt, Step r (nc + 1) (FVal $ lf |> t) (FVal xs) : st)
goRight _ = undefined

goLeft :: ZipTree a -> Maybe (ZipTree a)
goLeft (_, []) = Nothing
goLeft (t, Step r nc (FVal lf) (FVal rf) : st) =
  case Seq.viewr lf of
    Seq.EmptyR     -> Nothing
    (xs Seq.:> nt) -> Just (nt, Step r (nc - 1) (FVal xs) (FVal (t <| rf)) : st)
goLeft _ = undefined

seeChildren :: (TreeExpand m a) => ZipTree a -> m (ZipTree a)
seeChildren n@(Node _ (FVal _), _)  = return n
seeChildren n@(Node e FThunk, st)     = do
  children <- Seq.fromList . map (`Node` FThunk) <$> expChildren n
  return (Node e (FVal children), st)

downChild :: (TreeExpand m a) => Int -> ZipTree a -> MaybeT m (ZipTree a)
downChild k e@(Node r FThunk, stps) = do
  children <- lift $ Seq.fromList . map (`Node` FThunk) <$> expChildren e
  downChild k (Node r (FVal children), stps)
downChild k (Node r (FVal forest), stps) =
  if k >= length forest
     then fail ""
     else return (child, step : stps)
       where
         child = Seq.index forest k
         step = Step r k (FVal before) (FVal $ Seq.drop 1 after)
         (before, after) = Seq.splitAt k forest

downChildren :: (TreeExpand m a) => ZipTree a -> m [ZipTree a]
downChildren zt = map (fromMaybe []) $ runMaybeT $ do
  firstChild <- downChild 0 zt
  return $ accum goRight firstChild

pathFromRoot :: ZipTree a -> [Int]
pathFromRoot (_, stps) = reverse $ map (\(Step _ k _ _) -> k) stps

elemsFromRoot :: ZipTree a -> [a]
elemsFromRoot (_, stps) = reverse $ map (\(Step e _ _ _) -> e) stps

goPath :: (TreeExpand m a) => [Int] -> ZipTree a -> MaybeT m (ZipTree a)
goPath [] t = return t
goPath (x:xs) t = downChild x t >>= goPath xs

foldPath :: (TreeExpand m a) => (a -> a -> a) -> a -> [Int] -> ZipTree a -> MaybeT m a
foldPath f c [] t = return $ f c (root $ view t)
foldPath f c (x:xs) t = do
  child <- downChild x t
  foldPath f (f c (root $ view t)) xs child


-- movements for breadth search

goDownRightOfPath :: (TreeExpand m a) => ZipTree a -> [Int] -> MaybeT m (ZipTree a)
goDownRightOfPath zt p | pathFromRoot zt < truncP = fail ""
                       | pathFromRoot zt > truncP = downChild 0 zt
                       | otherwise                = downChild (n+1) zt
  where
    truncP = take (depth zt) p
    n = fromMaybe 0 (atMay p (depth zt))

goAbsRight :: (TreeExpand m a) => ZipTree a -> MaybeT m (ZipTree a)
goAbsRight zt = expl startPath zt
  where
    startPath = pathFromRoot zt
    tdepth = depth zt
    expl path z | depth z == tdepth && cPath > startPath = return z
                | otherwise                              = do
                    z' <- lift $ seeChildren z
                    let down = if depth z < tdepth
                                  then goDownRightOfPath z' path
                                  else fail ""
                    next <- down <|> (MaybeT . return) (upward z')
                    expl cPath next
      where
        cPath = pathFromRoot z

goDepthFarLeft :: (TreeExpand m a) => ZipTree a -> Int -> MaybeT m (ZipTree a)
goDepthFarLeft zt d = goFarLeft $ upToRoot zt
  where
    goFarLeft z =
      if depth z == d
         then return z
         else do
           firstChild <- downChild 0 z
           asum $ map goFarLeft (accum goRight firstChild)

breadthNext :: (TreeExpand m a) => ZipTree a -> MaybeT m (ZipTree a)
breadthNext zt =
  goAbsRight zt <|> goDepthFarLeft zt (depth zt + 1)

foldTree :: (TreeExpand m a) => (a -> b -> b) -> b -> Tree a -> m b
foldTree f x0 t = do
  l <- accumT breadthNext (fromTree t)
  return $ foldr (f . root .view) x0 l
