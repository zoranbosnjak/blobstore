{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Blob repository

module Repository
    ( module Repository
    , module System.Lock.FLock
    ) where

import           Turtle
import qualified Turtle.Bytes as TB
import           Prelude hiding (FilePath)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Control.Foldl as Fold
import           Control.Monad.Trans.Control
import           Control.Monad.Catch
import           System.Lock.FLock
import           System.IO.Temp
import           System.Process
import           System.IO hiding (FilePath)
import qualified Filesystem.Path.CurrentOS as Path
import           System.Posix.Files

type BlobHash = Text
type NestingLevel = Int

newtype Repository = Repository { unRepository :: FilePath }
    deriving (Eq, Show)

fileHashProcess :: CreateProcess
fileHashProcess = val
    { std_in = CreatePipe
    , std_out = CreatePipe
    }
  where
    val = System.Process.proc "b2sum" ["-b", "-l", "512"]

-- | Path to blobs directory.
blobs :: Repository -> FilePath
blobs (Repository repo) = repo </> "blobs"

-- | Path to secondary blobs directory (need for updating).
blobs2 :: Repository -> FilePath
blobs2 (Repository repo) = repo </> "blobs2"

-- | Path to lock file.
lockfile :: Repository -> FilePath
lockfile (Repository repo) = repo </> "lockfile"

-- | Path to temp directory.
tmp :: Repository -> FilePath
tmp (Repository repo) = repo </> "tmp"

-- | Path to nesting level configuration.
nestingConfig :: Repository -> FilePath
nestingConfig (Repository repo) = repo </> "nesting.config"

readNestingLevel :: MonadIO m => Repository -> m NestingLevel
readNestingLevel repo = (read . T.unpack) <$> liftIO (readTextFile $ nestingConfig repo)

-- | Calculate real filepath.
blobHashToFilePath :: NestingLevel -> BlobHash -> FilePath
blobHashToFilePath n val
    | n < 0 = error "negative nesting level"
    | n == 0 = fromText val
    | otherwise
        = (fromText $ T.take 2 val)
      </> blobHashToFilePath (pred n) (T.drop 2 val)

-- | Run action with locked repository.
withLockedRepo :: (MonadIO m, MonadBaseControl IO m) =>
    Repository -> SharedExclusive -> (Repository -> m a) -> m a
withLockedRepo repo lck act = withLock (encodeString $ lockfile repo) lck Block (act repo)

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

toText' :: FilePath -> Text
toText' val = case Path.toText val of
    Left _ -> error "unexpected filepath conversion"
    Right result -> result

-- | List blobs.
listBlobs :: Repository -> Shell BlobHash
listBlobs repo = do
    nestingLevel <- readNestingLevel repo
    go nestingLevel rootPath
  where
    rootPath = blobs repo
    rootPathLength = T.length $ toText' rootPath

    go 0 path = do
        val <- liftIO (sort (ls path)) >>= select
        return $ T.filter (/= '/') $ T.drop (succ rootPathLength) $ toText' val

    go n path = do
        val <- liftIO (sort (ls path)) >>= select
        isDir <- testdir val
        unless isDir $ die "unexpected file"
        go (pred n) val

-- | Set repository new nesting level.
adjustNesting :: Repository -> NestingLevel -> Shell ()
adjustNesting repo newNestingLevel = do
    when (newNestingLevel < 0) $ die "negative nesting level"
    oldNestingLevel <- readNestingLevel repo
    when (newNestingLevel /= oldNestingLevel) $ liftIO $ do
        mktree $ blobs2 repo
        sh $ do
            blob <- listBlobs repo
            let src = blobs repo </> blobHashToFilePath oldNestingLevel blob
                dst = blobs2 repo </> blobHashToFilePath newNestingLevel blob
            mktree $ directory dst
            mv src dst
        writeTextFile (nestingConfig repo) (T.pack $ show newNestingLevel)
        rmtree $ blobs repo
        mv (blobs2 repo) (blobs repo)

-- | Calculate bytestring hash, using external process.
hashFold :: FoldM IO BS.ByteString BlobHash
hashFold = FoldM step initial extract where
    step r@(hin, _, _) chunk = do
        BS.hPut hin chunk
        return r
    initial = do
        (Just hin, Just hout, _, ph) <- createProcess_ "hashBlobFold" fileHashProcess
        hSetBinaryMode hin True
        return (hin, hout, ph)
    extract (hin, hout, ph) = do
        hClose hin
        out <- hGetLine hout
        rv <- waitForProcess ph
        unless (rv == ExitSuccess) $ die "problems with hashing"
        return $ T.pack $ head $ words out

-- | Save bytes to a file as a fold.
saveFileFold :: FilePath -> FoldM IO BS.ByteString ()
saveFileFold target = FoldM step initial extract where
    step h chunk = BS.hPut h chunk >> return h
    initial = openFile (encodeString target) WriteMode
    extract h = hClose h

-- | Calculate hash and save file at the same time (combine folds).
importFold :: FilePath -> FoldM IO BS.ByteString BlobHash
importFold target = hashFold <* saveFileFold target

-- | Conditionally import blob.
-- Run finalizing action 'after' the blob is imported.
importBlobThen :: (MonadMask m, MonadIO m) =>
    Repository -> Shell BS.ByteString -> (BlobHash -> m (Either e (m ()))) -> m (Either e BlobHash)
importBlobThen repo i act = withTempDirectory (encodeString $ tmp repo) "importing" $ \td -> do
    let tempfile = decodeString td </> "tempfile"
    blobHash <- foldIO i (importFold tempfile)
    act blobHash >>= \case
        Left val -> do          -- blob is not needed
            rm tempfile
            return $ Left val
        Right finalizer -> do   -- blob needs to be stored
            nestingLevel <- readNestingLevel repo
            let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
            mktree $ directory target
            liftIO $ setFileMode (encodeString tempfile)
                (ownerReadMode `unionFileModes` groupReadMode `unionFileModes` otherReadMode)
            mv tempfile target
            finalizer
            return $ Right blobHash

-- | Import blob.
importBlob :: (MonadMask m, MonadIO m) =>
    Repository -> Shell BS.ByteString -> m BlobHash
importBlob repo i = do
    result <- importBlobThen repo i (\_ -> return (Right (return ())))
    case result of
        Left _ -> error "unexpected"
        Right val -> return val

-- | Hash blob only.
hashBlob :: MonadIO m => Shell BS.ByteString -> m BlobHash
hashBlob val = foldIO val hashFold

-- | Export blob.
exportBlob :: Repository -> BlobHash -> Shell BS.ByteString
exportBlob repo blobHash = do
    nestingLevel <- readNestingLevel repo
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    TB.input target

-- | Stat blob.
statBlob :: MonadIO m => Repository -> BlobHash -> m Size
statBlob repo blobHash = do
    nestingLevel <- readNestingLevel repo
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    du target

-- | Check if blob exists.
testBlob :: MonadIO m => Repository -> BlobHash -> m Bool
testBlob repo blobHash = do
    nestingLevel <- readNestingLevel repo
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    testfile target

-- | Delete blob.
deleteBlob :: MonadIO m => Repository -> BlobHash -> m ()
deleteBlob repo blobHash = do
    nestingLevel <- readNestingLevel repo
    let target = blobs repo </> blobHashToFilePath nestingLevel blobHash
    rm target
    -- TODO: remove empty directories

-- | Cleanup repository, get list of hashes to keep, remove the rest.
cleanup :: MonadIO m => Repository -> Shell BlobHash -> m ()
cleanup _repo _keep = do
    -- TODO
    undefined

