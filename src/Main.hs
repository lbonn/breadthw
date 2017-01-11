module Main where

import           Control.Exception
import           Control.Monad
import qualified Data.List           as L
import           Data.Sequence       (Seq, (><))
import qualified Data.Sequence       as Seq
import           Options.Applicative
import           Pipes
import           System.Directory
import           System.FilePath
import           System.IO

import           GHC.Unicode


data FileType = FTDir | FTFile | FTBoth deriving Show

data Opts = Opts {
  fileTypes  :: FileType,
  skipHidden :: Bool,
  startDir   :: FilePath
}

parseFileType :: Monad m => String -> m FileType
parseFileType "d" = return FTDir
parseFileType "f" = return FTFile
parseFileType "a" = return FTBoth
parseFileType _   = fail "invalid type"

optsP :: Parser Opts
optsP = Opts
  <$> option (str >>= parseFileType)
      ( long "file-type"
     <> short 't'
     <> help "File types to return (a|f|d)"
     <> value FTBoth
     <> metavar "FT" )
  <*> switch
      ( long "skip-hidden"
     <> short 's'
     <> help "Whether to walk into hidden dirs/files" )
  <*> argument str
      ( metavar "DIR"
     <> help "Starting directory"
     <> value "." )

matchFt :: FileType -> FilePath -> IO Bool
matchFt FTDir  = doesDirectoryExist
matchFt FTFile = doesFileExist
matchFt FTBoth = \_ -> return True

isHidden :: FilePath -> Bool
isHidden p = case takeFileName p of
  '.' : _ : _ -> True
  _           -> False

escapeWronglyEncoded :: String -> String
escapeWronglyEncoded = map esc where
  esc c = if isPrint c then c
    else '\65533'  -- Unicode REPLACEMENT CHARACTER

matchOutputConds :: Opts -> FilePath -> IO Bool
matchOutputConds opts p =
  if not $ all isPrint p then do
    hPutStrLn stderr $ "bad encoding: " ++ escapeWronglyEncoded p
    return False
  else do
    let fnMatch = not (skipHidden opts) || not (isHidden p)
    if fnMatch then matchFt (fileTypes opts) p else return False


walkOnce :: FilePath -> IO [FilePath]
walkOnce p = do
  isDir <- doesDirectoryExist p
  if not isDir then return [] else do
    l <- try $ listDirectory p :: IO (Either IOException [FilePath])

    case l of
      Left e -> do
        hPrint stderr e
        return []
      Right rl -> return $ map (combine p) (L.sort rl)


walkDir :: Opts -> Seq FilePath -> Producer FilePath IO ()
walkDir opts = walkd where
  walkd q = case Seq.viewl q of
    Seq.EmptyL -> return ()
    (x Seq.:< xs) -> do
      match <- liftIO $ matchOutputConds opts x
      children <- liftIO $ if match then walkOnce x else return []

      when (match && x /= startDir opts) (yield x)

      walkd $ xs >< Seq.fromList children


formatPath :: FilePath -> String
formatPath = normalise

mainWalk :: Opts -> IO ()
mainWalk opts = runEffect $ for walk disp
  where
    walk = walkDir opts (Seq.fromList [startDir opts])
    disp = lift . putStrLn . formatPath


main :: IO ()
main = execParser opts >>= mainWalk
  where
    opts = info (helper <*> optsP) fullDesc
