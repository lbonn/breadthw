module Main where

import           Control.Exception
import qualified Data.List                   as L
import qualified Data.Sequence               as Seq
import           Options.Applicative
import           Options.Applicative.Builder()
import           System.Directory
import           System.FilePath

import           System.IO
import           System.IO.Unsafe

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

type Queue a = Seq.Seq a

matchFt :: FileType -> FilePath -> IO Bool
matchFt FTDir  = doesDirectoryExist
matchFt FTFile = doesFileExist
matchFt FTBoth = \_ -> return True

isHidden :: FilePath -> Bool
isHidden p = case takeFileName p of
  '.' : _ -> True
  _ -> False

matchOutputConds :: Opts -> FilePath -> IO Bool
matchOutputConds opts p = do
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


walkDir :: Opts -> Queue FilePath -> IO [FilePath]
walkDir opts = walkd where
  walkd q = case Seq.viewl q of
    Seq.EmptyL -> return []
    (x Seq.:< xs) -> do
      match <- matchOutputConds opts x
      children <- if match then walkOnce x else return []

      -- hum?: https://stackoverflow.com/questions/16243789/create-lazy-io-list-from-a-non-io-list
      w <- unsafeInterleaveIO $ walkd $ xs Seq.>< Seq.fromList children

      return $ if match && x /= startDir opts then x : w else w


mainWalk :: Opts -> IO ()
mainWalk opts = do
  res <- walkDir opts (Seq.fromList [startDir opts])
  mapM_ putStrLn res


main :: IO ()
main = execParser opts >>= mainWalk
  where
    opts = info (helper <*> optsP) fullDesc
