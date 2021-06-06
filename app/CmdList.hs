{-# LANGUAGE OverloadedStrings #-}

module CmdList where

import           Turtle
import           Prelude hiding (FilePath)

import           Command
import           Repository

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { cmdRepo :: Repository
    } deriving (Eq, Show)

-- | Option parser.
parseOptions :: Parser CmdOptions
parseOptions = CmdOptions
    <$> fmap Repository (optPath "repository" 'R' "Repository path")

parser :: CommandName -> Parser Command
parser name = runCmd <$> subcommand name "List blobs" parseOptions

runCmd :: CmdOptions -> Command
runCmd cmd _problem _ctx = do
    repo <- using $ managed (withLockedRepo (cmdRepo cmd) Exclusive)
    b <- listBlobs repo
    echo $ unsafeTextToLine b

