{-# LANGUAGE OverloadedStrings #-}

module CmdExport where

import           Turtle
import           Turtle.Bytes as TB
import           Prelude hiding (FilePath)

import qualified Data.Text as T

import           Command
import           Repository

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { cmdRepo :: Repository
    , cmdBlob :: Text
    , cmdStat :: Bool
    } deriving (Eq, Show)

-- | Option parser.
parseOptions :: Parser CmdOptions
parseOptions = CmdOptions
    <$> fmap Repository (optPath "repository" 'R' "Repository path")
    <*> argText "BLOB" "blob hash value"
    <*> switch "stat" 'n' "don't import, just print size"

parser :: CommandName -> Parser Command
parser name = runCmd <$> subcommand name "Export blob" parseOptions

runCmd :: CmdOptions -> Command
runCmd cmd _problem _ctx = do
    let blobHash = T.unpack $ cmdBlob cmd
    repo <- using $ managed (withLockedRepo (cmdRepo cmd) Shared)
    case cmdStat cmd of
        False -> TB.stdout $ exportBlob repo blobHash
        True -> do
            size <- statBlob repo blobHash
            liftIO $ print size

