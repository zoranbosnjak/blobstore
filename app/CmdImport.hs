{-# LANGUAGE OverloadedStrings #-}

module CmdImport where

import           Turtle
import           Turtle.Bytes as TB
import           Prelude hiding (FilePath)

import           Command
import           Repository

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { cmdRepo :: Repository
    , cmdFiles :: [FilePath]
    , cmdDont :: Bool
    } deriving (Eq, Show)

-- | Option parser.
parseOptions :: Parser CmdOptions
parseOptions = CmdOptions
    <$> fmap Repository (optPath "repository" 'R' "Repository path")
    <*> many (argPath "path" "path to import")
    <*> switch "no-import" 'n' "don't import, just show hash"

parser :: CommandName -> Parser Command
parser name = runCmd <$> subcommand name "Import blob(s)" parseOptions

runCmd :: CmdOptions -> Command
runCmd cmd _problem _ctx = do
    file <- case cmdFiles cmd of
        [] -> return TB.stdin
        val -> mapM getFileListing val >>= select >>= return . TB.input
    blobHash <- liftIO $ act file
    echo $ unsafeTextToLine blobHash
  where
    act = case cmdDont cmd of
        False -> \val -> withLockedRepo (cmdRepo cmd) Shared $ \repo -> importBlob repo val
        True -> hashBlob

