{-# LANGUAGE OverloadedStrings #-}

module CmdInit where

import           Turtle
import           Prelude hiding (FilePath)

import           Options.Applicative as Opt

import           Command
import           Repository

-- | Speciffic command options.
data CmdOptions = CmdOptions
    { cmdRepo :: FilePath
    , cmdNesting :: NestingLevel
    } deriving (Eq, Show)

-- | Option parser.
parseOptions :: Parser CmdOptions
parseOptions = CmdOptions
    <$> argPath "repo" "Repository path"
    <*> Opt.option auto
        ( long "nesting"
       <> help "File nesting level"
       <> showDefault
       <> value 0
       <> metavar "INT"
        )

parser :: CommandName -> Parser Command
parser name = runCmd <$> subcommand name "Init repository" parseOptions

runCmd :: CmdOptions -> Command
runCmd cmd _problem _ctx = do
    initRepo (Repository $ cmdRepo cmd) (cmdNesting cmd)

