{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- {-# OPTIONS_GHC -Wno-unused-imports #-}

-- Blob repository

module Repository
    ( module Repository
    , module System.Lock.FLock
    ) where

import           Turtle
import qualified Turtle.Bytes as TB
import           Prelude hiding (FilePath)

import           Crypto.Hash

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Base16 as B16

import qualified Data.ByteArray as BA

import qualified Data.Text as T
import qualified Control.Foldl as Fold
import           Control.Monad.Trans.Control
import           Control.Monad.Catch
import           System.Lock.FLock
import           System.IO.Temp

type NestingLevel = Int

type Alg = SHA256

type BlobHash = BS.ByteString

digestToBS :: Digest Alg -> BS.ByteString
digestToBS = BS.pack . BA.unpack

newtype Repository = Repository { unRepository :: FilePath }
    deriving (Eq, Show)

-- | Path to blobs directory.
blobs :: Repository -> FilePath
blobs (Repository repo) = repo </> "blobs"

-- | Path to lock file.
lockfile :: Repository -> FilePath
lockfile (Repository repo) = repo </> "lockfile"

-- | Path to temp directory.
tmp :: Repository -> FilePath
tmp (Repository repo) = repo </> "tmp"

-- | Path to nesting level configuration.
nestingConfig :: Repository -> FilePath
nestingConfig (Repository repo) = repo </> "nesting.config"

-- | Convert bytestring to hex representation.
hexlify :: BS.ByteString -> String
hexlify = BS8.unpack . B16.encode

-- | Convert hex representation back to a bytestring.
unhexlify :: String -> Maybe BS.ByteString
unhexlify st = do
    let (a,b) = B16.decode $ BS8.pack st
    guard $ BS.null b
    return a

-- | Calculate real filepath.
blobHashToFilePath :: NestingLevel -> BlobHash -> FilePath
blobHashToFilePath n val
    | n < 0 = error "negative nesting level"
    | n == 0 = toPath val
    | otherwise
        = (toPath $ BS.take 1 val)
      </> blobHashToFilePath (pred n) (BS.drop 1 val)
  where
    toPath = decodeString . hexlify

-- | Run action with locked repository.
withLockedRepo :: (MonadIO m, MonadBaseControl IO m) =>
    Repository -> SharedExclusive -> m a -> m a
withLockedRepo repo lck = withLock (encodeString $ lockfile repo) lck Block

-- | Initialize repository.
initRepo :: MonadIO m => Repository -> NestingLevel -> m ()
initRepo repo nestingLevel = do
    mktree $ unRepository repo
    isEmpty <- Turtle.fold (ls $ unRepository repo) Fold.null
    unless isEmpty $ die "directory not empty"
    touch $ lockfile repo
    mktree $ blobs repo
    mktree $ tmp repo
    liftIO $ writeTextFile (nestingConfig repo) (T.pack $ show nestingLevel)

-- | Set repository nesting level.
adjustNesting :: MonadIO m => Repository -> NestingLevel -> m ()
adjustNesting _repo _newNesting = do
    undefined

-- | Calculate hash fold, without actually storing blob.
_hashBlobFold :: Fold BS.ByteString BlobHash
_hashBlobFold = Fold hashUpdate hashInit (digestToBS . hashFinalize)

-- | Calculate hash, without actually storing blob.
hashBlob :: MonadIO m => Shell BS.ByteString -> m BlobHash
hashBlob = flip fold _hashBlobFold

-- | Import blob fold.
_importBlobFold :: FilePath -> FoldShell BS.ByteString BlobHash
_importBlobFold tempfile = FoldShell update hashInit finalize
  where
    update acc chunk = do
        TB.append tempfile (return chunk)
        return (hashUpdate acc chunk)
    finalize = return . digestToBS. hashFinalize

-- | Import blob.
importBlob :: (MonadMask m, MonadIO m) =>
    Repository -> Shell BS.ByteString -> m BlobHash
importBlob repo val = withTempDirectory (encodeString $ tmp repo) "importing" $ \td -> do
    nestingLevel <- (read . T.unpack) <$> liftIO (readTextFile $ nestingConfig repo)
    let tempfile = decodeString td </> "tempfile"
    touch tempfile
    blobHash <- foldShell val (_importBlobFold tempfile)
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    mktree $ directory target
    mv tempfile target
    return blobHash

-- | Export blob.
exportBlob :: Repository -> BlobHash -> Shell BS.ByteString
exportBlob repo blobHash = do
    nestingLevel <- (read . T.unpack) <$> liftIO (readTextFile $ nestingConfig repo)
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    TB.input target

-- | Enumerate blobs.
enumerateBlobs :: Repository -> Shell BlobHash
enumerateBlobs _repo = do
    undefined

-- | Stat blob.
statBlob :: MonadIO m => Repository -> BlobHash -> m Size
statBlob repo blobHash = do
    nestingLevel <- (read . T.unpack) <$> liftIO (readTextFile $ nestingConfig repo)
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    du target

-- | Delete blob.
deleteBlob :: MonadIO m => Repository -> BlobHash -> m ()
deleteBlob repo blobHash = do
    nestingLevel <- (read . T.unpack) <$> liftIO (readTextFile $ nestingConfig repo)
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    rm target
    -- TODO: remove empty directory

-- | Cleanup repository, get list of hashes to keep, remove the rest.
cleanup :: Repository -> Shell BlobHash -> m ()
cleanup _repo _keep = do
    undefined

