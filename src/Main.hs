module Main where

import           Protolude           hiding ((<>))

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import qualified Data.List                 as L
import           Data.Monoid
import           Data.Sequence             (Seq, (><))
import qualified Data.Sequence             as Seq
import qualified Data.Text                 as T
import qualified Data.Text.IO
import qualified Options.Applicative       as OA
import           Pipes                     hiding (for)
import qualified Pipes                     as P
import           System.Directory
import           System.IO                 (hPutStrLn)

import           GHC.Unicode


type TextPath = Text
data FileType = FTDir | FTFile | FTBoth deriving Show

data Opts = Opts {
  fileTypes  :: FileType,
  skipHidden :: Bool,
  startDir   :: TextPath
}

parseFileType ::  (Monad m, IsString a, Eq a) => a -> m FileType
parseFileType "d" = return FTDir
parseFileType "f" = return FTFile
parseFileType "a" = return FTBoth
parseFileType _   = fail "invalid type"

optsP :: OA.Parser Opts
optsP = Opts
  <$> OA.option (OA.str >>= parseFileType)
      ( OA.long "file-type"
     <> OA.short 't'
     <> OA.help "File types to return (a|f|d)"
     <> OA.value FTBoth
     <> OA.metavar "FT" )
  <*> OA.switch
      ( OA.long "skip-hidden"
     <> OA.short 's'
     <> OA.help "Whether to walk into hidden dirs/files" )
  <*> OA.argument (map T.pack OA.str)
      ( OA.metavar "DIR"
     <> OA.help "Starting directory"
     <> OA.value (T.pack ".") )


-- some path utils
fileName :: TextPath -> TextPath
fileName = T.takeWhileEnd (/= '/')

combine :: TextPath -> TextPath -> TextPath
combine p1 p2 = if "/" `T.isSuffixOf` p1
  then
    p1 <> p2
  else
    p1 <> "/" <> p2

isHidden :: TextPath -> Bool
isHidden p = case T.stripPrefix "." $ fileName p of
  Nothing -> False
  Just "" -> False
  Just _  -> True

-- escape bad unicode to avoid runtime error at print
escapeWronglyEncoded :: Text -> Text
escapeWronglyEncoded = T.map esc where
  esc c = if isPrint c then c
    else '\65533'  -- Unicode REPLACEMENT CHARACTER

-- validate a candidate for output (or not)
matchOutputConds :: Opts -> TextPath -> IO Bool
matchOutputConds opts p = map isJust $ runMaybeT $ do
  -- is it badly encoded?
  unless (T.all isPrint p) (do
      lift $ hPutStrLn stderr . T.unpack $ "bad encoding: " `mappend` escapeWronglyEncoded p
      fail "badly encoded"
      )

  -- is it hidden and hidden files are skipped
  when (skipHidden opts && isHidden p) $ fail "hidden file"

  -- is it the correct file type?
  mft <- liftIO $ matchFt (fileTypes opts) p
  unless mft $ fail "unwanted file type"

  where
    matchFt :: FileType -> TextPath -> IO Bool
    matchFt FTDir  = doesDirectoryExist . T.unpack
    matchFt FTFile = doesFileExist . T.unpack
    matchFt FTBoth = \_ -> return True


-- walk one level into a dir at once
walkOnce :: TextPath -> IO [TextPath]
walkOnce p = do
  isDir <- doesDirectoryExist up
  if not isDir then return [] else do
    l <- try $ listDirectory up :: IO (Either IOException [FilePath])

    case l of
      Left e -> do
        hPutStrLn stderr (T.unpack . show $ e)
        return []
      Right rl -> return $ map (combine p) (L.sort trl)
        where
          trl = map T.pack rl
  where
    up = T.unpack p


-- recursive walk
walkDir :: Opts -> Seq TextPath -> Producer TextPath IO ()
walkDir opts = walkd where
  walkd q = case Seq.viewl q of
    Seq.EmptyL -> return ()
    (x Seq.:< xs) -> do
      match <- liftIO $ matchOutputConds opts x
      children <- liftIO $ if match then walkOnce x else return []

      when (match && x /= startDir opts) (yield x)

      walkd $ xs >< Seq.fromList children


-- small cosmetic arrangement
formatPath :: TextPath -> TextPath
formatPath p = p `fromMaybe` T.stripPrefix "./" p


mainWalk :: Opts -> IO ()
mainWalk opts = runEffect $ P.for walk disp
  where
    walk = walkDir opts (Seq.fromList [startDir opts])
    disp = lift . putStrLn . formatPath

main :: IO ()
main = OA.execParser opts >>= mainWalk
  where
    opts = OA.info (OA.helper <*> optsP) OA.fullDesc
