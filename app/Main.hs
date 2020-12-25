{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Main where

import Turtle
import Prelude hiding (FilePath)

import           Options.Applicative as Opt

import           Paths_blobstore (version)
import           Data.Version (showVersion)
import           Data.IORef
import           Data.Foldable (asum)
import           System.Environment (getProgName, getArgs)

-- local imports
import           Command
import qualified CmdInit
import qualified CmdImport
import qualified CmdExport

commands :: [Parser Command]
commands =
    [ CmdInit.parser "init"
    , CmdImport.parser "import"
    , CmdExport.parser "export"
    ]

data Options = Options
    { optCommand :: Command
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> asum commands

opts :: ParserInfo Options
opts = info (helper <*> versionOption <*> parseOptions)
    ( fullDesc <> Opt.header "Blob store" )
  where
    versionOption = Opt.infoOption
        (showVersion version)
        (Opt.long "version" <> Opt.help "Show version")

main :: IO ()
main = do
    pName <- getProgName
    pArgs <- getArgs
    cmdOptions <- execParser opts

    let ctx = Ctx
            { ctxProg = pName
            , ctxArgs = pArgs
            -- , ctxRepository = undefined
            }

    failureFlag <- newIORef False
    let setFlag :: MonadIO m => m ()
        setFlag = liftIO $ writeIORef failureFlag True

        getFlag :: MonadIO m => m Bool
        getFlag = liftIO $ readIORef failureFlag

    let problem msg = do
            eprintf (""%s%"\n") msg
            setFlag

    sh $ (optCommand cmdOptions) problem ctx
    getFlag >>= \case
        False -> return ()
        True -> exit $ ExitFailure 1

