module Main where

import           Protolude hiding ((<>))

import           Control.Exception
import           Control.Monad
import qualified Data.List           as L
import           Data.Monoid
import           Data.Sequence (Seq, (><))
import qualified Data.Sequence       as Seq
import qualified Data.Text           as T
import qualified Data.Text.IO
import qualified Options.Applicative as OA
import           Pipes               hiding (for)
import qualified Pipes               as P
import           System.Directory
import           System.FilePath
import           System.IO (hPutStrLn)

import           GHC.Unicode


type TextPath = Text
data FileType = FTDir | FTFile | FTBoth deriving Show

data Opts = Opts {
  fileTypes  :: FileType,
  skipHidden :: Bool,
  startDir   :: TextPath
}

parseFileType ::  Monad m => [Char] -> m FileType
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

matchFt :: FileType -> Text -> IO Bool
matchFt FTDir  = doesDirectoryExist . T.unpack
matchFt FTFile = doesFileExist . T.unpack
matchFt FTBoth = \_ -> return True

-- TODO: reimplement
isHidden :: Text -> Bool
isHidden p = case takeFileName $ T.unpack p of
  '.' : _ : _ -> True
  _           -> False

escapeWronglyEncoded :: Text -> Text
escapeWronglyEncoded = T.map esc where
  esc c = if isPrint c then c
    else '\65533'  -- Unicode REPLACEMENT CHARACTER

matchOutputConds :: Opts -> Text -> IO Bool
matchOutputConds opts p =
  if not $ T.all isPrint p then do
    hPutStrLn stderr . T.unpack $ "bad encoding: " `mappend` escapeWronglyEncoded p
    return False
  else do
    let fnMatch = not (skipHidden opts) || not (isHidden p)
    if fnMatch then matchFt (fileTypes opts) p else return False


walkOnce :: Text -> IO [Text]
walkOnce p = do
  isDir <- doesDirectoryExist up
  if not isDir then return [] else do
    l <- try $ listDirectory up :: IO (Either IOException [FilePath])

    case l of
      Left e -> do
        hPutStrLn stderr (T.unpack . show $ e)
        return []
      Right rl -> return $ map ((T.pack . combine up) . T.unpack)  (L.sort trl)
        where
          trl = map T.pack rl
  where
    up = T.unpack p


walkDir :: Opts -> Seq Text -> Producer Text IO ()
walkDir opts = walkd where
  walkd q = case Seq.viewl q of
    Seq.EmptyL -> return ()
    (x Seq.:< xs) -> do
      match <- liftIO $ matchOutputConds opts x
      children <- liftIO $ if match then walkOnce x else return []

      when (match && x /= startDir opts) (yield x)

      walkd $ xs >< Seq.fromList children


formatPath :: Text -> Text
formatPath = T.pack . normalise . T.unpack

mainWalk :: Opts -> IO ()
mainWalk opts = runEffect $ P.for walk disp
  where
    walk = walkDir opts (Seq.fromList [startDir opts])
    disp = lift . putStrLn . formatPath


main :: IO ()
main = OA.execParser opts >>= mainWalk
  where
    opts = OA.info (OA.helper <*> optsP) OA.fullDesc
