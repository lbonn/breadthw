module Main where

import           Protolude
import           Prelude             (String)

import           Control.Monad
import qualified Data.Text           as T
import           Options.Applicative
import qualified Pipes               as P

import           Breadthw


parseFileType ::  (IsString a, Eq a) => a -> Either String FileType
parseFileType "d" = return FTDir
parseFileType "f" = return FTFile
parseFileType "a" = return FTBoth
parseFileType _   = fail "type should be \"a\", \"f\" or \"d\""

optsP :: Parser Opts
optsP = Opts
  <$> option (eitherReader parseFileType)
      ( long "file-type"
     <> short 't'
     <> help "File types to return (a|f|d)"
     <> value FTBoth
     <> metavar "FT" )
  <*> switch
      ( long "skip-hidden"
     <> short 's'
     <> help "Whether to walk into hidden dirs/files" )
  <*> switch
      ( long "follow-symlink"
     <> short 'f'
     <> help "Whether to follow symlinks (dangerous)" )
  <*> argument (map T.pack str)
      ( metavar "DIR"
     <> help "Starting directory"
     <> value (T.pack ".") )


mainWalk :: Opts -> IO ()
mainWalk opts = P.runEffect $ P.for (walkDir opts) disp
  where
    disp = lift . putStrLn . formatPath

main :: IO ()
main = execParser opts >>= mainWalk
  where
    opts = info (helper <*> optsP) fullDesc
