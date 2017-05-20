{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Breadthw
  (module Breadthw) where

import           Protolude

import           Control.Monad.Fail        (fail)
import           Control.Monad.Reader      (runReaderT)
import           Control.Monad.Trans.Maybe (runMaybeT)
import qualified Data.List                 as L
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as T
import           GHC.Unicode               (isPrint)
import           Pipes                     (Producer, yield)
import           System.Directory          (doesDirectoryExist, doesFileExist,
                                            listDirectory, pathIsSymbolicLink)
import           System.IO                 (hPutStrLn)


import           Breadthw.ZipTree

type TextPath = Text
data FileType = FTDir | FTFile | FTBoth deriving Show

data Opts = Opts {
  fileTypes      :: FileType,
  zipAl          :: Bool,
  skipHidden     :: Bool,
  followSymlinks :: Bool,
  startDir       :: TextPath
}

-- some path utils
fileName :: TextPath -> TextPath
fileName = T.takeWhileEnd (/= '/')

combine :: TextPath -> TextPath -> TextPath
combine p1 "" = p1
combine "" p2 = p2
combine p1 p2 =
  if "/" `T.isSuffixOf` p1
     then p1 <> p2
     else p1 <> "/" <> p2

isHidden :: TextPath -> Bool
isHidden p =
  case T.stripPrefix "." $ fileName p of
    Nothing -> False
    Just "" -> False
    Just _  -> True

-- escape bad unicode to avoid runtime error at print
escapeWronglyEncoded :: Text -> Text
escapeWronglyEncoded = T.map esc where
  esc c =
    if isPrint c
       then c
       else '\65533'  -- Unicode REPLACEMENT CHARACTER

-- validate a candidate for output (or not)
matchFt :: FileType -> TextPath -> IO Bool
matchFt FTDir  = doesDirectoryExist . T.unpack
matchFt FTFile = doesFileExist . T.unpack
matchFt FTBoth = \_ -> return True

-- | If this doesn't match, children should not be explored
matchInterConds :: Opts -> TextPath -> IO Bool
matchInterConds opts p = map isJust $ runMaybeT $ do
  -- is it badly encoded?
  unless (T.all isPrint p) $ do
    lift $ hPutStrLn stderr . T.unpack $ "bad encoding: " `mappend` escapeWronglyEncoded p
    fail "badly encoded"

  -- is it hidden and hidden files are skipped
  when (skipHidden opts && isHidden p) $ fail "hidden file"

-- | If this doesn't match, it should not be output
matchOutputConds :: Opts -> TextPath -> IO Bool
matchOutputConds opts p = liftIO $ matchFt (fileTypes opts) p

-- walk one level into a dir at once
walkOnce :: Opts -> TextPath -> IO [TextPath]
walkOnce opts p = map (fromMaybe []) $ runMaybeT $ do
  -- is it a directory?
  isDir <- liftIO $ doesDirectoryExist pR
  unless isDir $ fail "not a directory"

  -- is it a symlink?
  unless (followSymlinks opts) $ do
    isSymlink <- liftIO $ pathIsSymbolicLink pR
    when isSymlink $ fail "symlink"

  l <- liftIO (try $ listDirectory pR :: IO (Either IOException [FilePath]))

  case l of
    Left e -> do
      liftIO $ hPutStrLn stderr (T.unpack . show $ e)
      return []
    Right rl ->
      return $ L.sort . map T.pack $ rl
  where
    pR = T.unpack p


-- recursive walk
walkDir :: Opts -> Producer TextPath IO ()
walkDir opts = walkd initWalk where
  initWalk = Seq.fromList [startDir opts]

  walkd :: Seq TextPath  -> Producer TextPath IO ()
  walkd q =
    case Seq.viewl q of
      Seq.EmptyL -> return ()
      (x Seq.:< xs) -> do
        shouldExpl <- liftIO $ matchInterConds opts x
        children <- liftIO $ if shouldExpl then map (combine x) <$> walkOnce opts x else return []

        shouldOutput <- liftIO $ map (shouldExpl &&) (matchOutputConds opts x)

        when (shouldOutput && x /= startDir opts) (yield x)

        walkd $ xs Seq.>< Seq.fromList children

-- ZipTree version

currentNodePath :: ZipTree TextPath -> TextPath
currentNodePath zt = foldr' combine "" (elemsFromRoot zt) `combine` (root . view $ zt)

newtype OptsIO a = OptsIO (ReaderT Opts IO a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Opts)

instance TreeExpand OptsIO TextPath where
  expChildren zt = do
    let fPath = currentNodePath zt
    opts <- ask

    children <- liftIO $ walkOnce opts fPath
    liftIO $ filterM (matchInterConds opts . (fPath `combine`)) children

runOptsIO :: Opts -> OptsIO a -> IO a
runOptsIO opts (OptsIO act) = runReaderT act opts

walkDirZip :: Opts -> Producer TextPath IO ()
walkDirZip opts = walkd startPoint
  where
    startPoint = fromTree (Node (startDir opts) FThunk)

    walkd :: ZipTree TextPath -> Producer TextPath IO ()
    walkd zt = do
      let fPath = currentNodePath zt

      match <- liftIO $ matchOutputConds opts fPath

      when (match && fPath /= startDir opts) (yield fPath)

      -- elect next path
      next <- liftIO $ (runOptsIO opts . runMaybeT) (breadthNext zt)
      maybe (return ()) walkd next

-- small cosmetic arrangement
formatPath :: TextPath -> TextPath
formatPath p = p `fromMaybe` T.stripPrefix "./" p
