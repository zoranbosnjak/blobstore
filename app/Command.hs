{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module Command where

import           Turtle
import           Prelude hiding (FilePath)

-- | Running environment
data Ctx = Ctx
    { ctxProg               :: String
    , ctxArgs               :: [String]
    -- , ctxRepository         :: Maybe FilePath
    } deriving (Show)

type Command = (Text -> Shell ()) -> Ctx -> Shell ()

