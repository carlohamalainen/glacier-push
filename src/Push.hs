{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE BangPatterns           #-}

module Push where

import GHC.Generics (Generic)
import Control.DeepSeq
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Trans.AWS              (AWST', runAWST)
import Control.Monad.Trans.Resource
import Data.Int
import Data.Either
import Data.Maybe
import Data.String.Conversions              (cs)
import Data.Text                            (append, Text)
import Katip

import System.Exit
import System.IO
import Text.Printf

import qualified Data.ByteString.Lazy           as BS
import qualified Data.Text.Lazy.Builder         as Builder
import qualified Data.CaseInsensitive           as CI

import TreehashFFI

import qualified Amazonka as AWS
import qualified Amazonka.Glacier as Glacier
import qualified Amazonka.Glacier.Lens                      as Glacier
import qualified Amazonka.Glacier.InitiateMultipartUpload   as Glacer

type Hash = BS.ByteString

chunkSize :: Int64
chunkSize = 4096

-- | Section of a file for a multipart upload. The start
-- and end offsets are inclusive.
data Part = Part
    { _partStart    :: !Int64
    , _partEnd      :: !Int64
    , _partHash     :: !Hash
    , _path         :: !FilePath
    }
  deriving (Generic, NFData, Show)

-- | Things needed for a multipart upload.
data MultiPart = MultiPart
    { _multipartFullHash    :: !Hash         -- ^ Hash of the whole file.
    , _partSize             :: !Int64        -- ^ Part size in bytes.
    , _multiParts           :: ![Part]       -- ^ Description of each part.
    , _multipartPath        :: !FilePath     -- ^ Path to the file.
    , _multipartArchiveSize :: !Int64        -- ^ Total archive (file) size.
    }
  deriving (Generic, NFData, Show)

makeLenses ''Part
makeLenses ''MultiPart

-- | We use a single '@-@ ' (hyphen) as the account ID so that Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request.
accountId :: Text
accountId = "-"

data MyException = MissingUploadID deriving (Show, Typeable)

instance Exception MyException

getFileSize' :: (MonadCatch m, MonadIO m) => FilePath -> m Integer
getFileSize' f = liftIO $ withFile f ReadMode hFileSize

oneMb :: Int64
oneMb = 1024*1024

send'
    :: (MonadUnliftIO m, AWS.AWSRequest a)
    => AWS.Env
    -> a
    -> m (AWS.AWSResponse a)
send' env x = AWS.runResourceT $ AWS.send env x

-- | Upload an archive in one go.
uploadArchiveSingle
    :: (MonadCatch m, MonadIO m, MonadUnliftIO m)
    => AWS.Env                  -- ^ AWS Environment.
    -> Text                     -- ^ Vault name.
    -> FilePath                 -- ^ File to upload.
    -> Text                     -- ^ Archive Description.
    -> m Glacier.ArchiveCreationOutput  -- ^ Response.
uploadArchiveSingle env vault f archiveDesc = do
    hash <- liftIO $ treehash_FFI' f
    body <- AWS.toHashed <$> (liftIO . BS.readFile) f

    let ua = Glacier.newUploadArchive vault accountId body
                & Glacier.uploadArchive_checksum            ?~ cs hash
                & Glacier.uploadArchive_archiveDescription  ?~ cs archiveDesc

    send' env ua

-- | Initiate a multipart upload. We don't need the
-- file name here, just its size.
initiateMulti
    :: (MonadCatch m, MonadIO m, MonadUnliftIO m)
    => AWS.Env                              -- ^ AWS Environment.
    -> Text                                 -- ^ Vault name.
    -> Int64                                -- ^ Part size (bytes).
    -> Text                                 -- ^ Archive Description.
    -> m Glacier.InitiateMultipartUploadResponse    -- ^ Response.
initiateMulti env vault _partSize archiveDesc = send' env mpu
  where
    mpu = Glacier.newInitiateMultipartUpload accountId vault (cs $ show _partSize)
            & Glacier.initiateMultipartUpload_archiveDescription ?~ cs archiveDesc

-- | Helper for constructing a 'MultiPart'. Given the part size,
-- this computes the hashes and part boundaries.
mkMultiPart
    :: forall m. (MonadCatch m, MonadIO m)
    => FilePath         -- ^ File.
    -> Int64            -- ^ Part size (bytes).
    -> Text             -- ^ Archive Description.
    -> m MultiPart
mkMultiPart _multipartPath _partSize _archiveDesc = do -- FIXME archive description is unused?...
    _multipartFullHash <- cs <$> liftIO (treehash_FFI' _multipartPath)

    liftIO $ print ("_multipartFullHash"::Text, _multipartFullHash)

    _multipartArchiveSize <- fromIntegral <$> getFileSize' _multipartPath

    let startEnds :: [(Int64, Int64)]
        startEnds = concat $ f (0::Int64) _multipartArchiveSize

    _multiParts <- forM startEnds mkPart

    return MultiPart{..}

  where

    mkPart :: (Int64, Int64) -> m Part
    mkPart (_partStart, _partEnd) = do
        _partHash <- cs <$> liftIO (treehash_FFI _multipartPath _partStart _partEnd)

        liftIO $ print ("part"::Text, show _partStart, show _partEnd, _partHash)

        let _path = _multipartPath
        return Part{..}

    f :: Int64 -> Int64 -> [[(Int64, Int64)]]
    f offset remaining
        | remaining <= _partSize = [[(offset, offset + remaining - 1)]]
        | otherwise              = [(offset, offset + _partSize - 1)] : f (offset + _partSize) (remaining - _partSize)

-- | Read a part of a file.
getPart
    :: (MonadCatch m, MonadIO m)
    => FilePath         -- ^ File to read.
    -> (Int64, Int64)   -- ^ First and last byte to read (inclusive).
    -> m BS.ByteString
getPart path' (start, end) = liftIO $ do
    h <- openFile path' ReadMode
    hSeek h AbsoluteSeek (fromIntegral start)
    x <- BS.hGet h $ fromIntegral $ end - start + 1
    hClose h
    return x

uploadOnePart
    :: (MonadCatch m, MonadIO m, MonadUnliftIO m)
    => AWS.Env                              -- ^ AWS Environment.
    -> Text                                 -- ^ Vault.
    -> Glacier.InitiateMultipartUploadResponse -- ^ Initiated multipart upload.
    -> Part                                 -- ^ A single part.
    -> Int                                  -- ^ Index of this part.
    -> Int                                  -- ^ Total number of parts.
    -> KatipContextT m Glacier.UploadMultipartPartResponse
uploadOnePart env vault mu p thisPartIndex nrParts = do
    let Part{..} = p

    body <- AWS.toHashed <$> getPart _path (_partStart, _partEnd)

    uploadId <- case mu ^. Glacier.initiateMultipartUploadResponse_uploadId of
                    Nothing     -> throw MissingUploadID
                    Just uid    -> return uid

    -- To avoid scientific notation in the Katip logs.
    let start, end :: Text
        start = cs $ show _partStart
        end   = cs $ show _partEnd

        pc :: Double
        pc = fromIntegral thisPartIndex / fromIntegral nrParts

        pc' :: Int
        pc' = floor $ 100*pc

        percentageComplete :: String
        percentageComplete = printf "%02d" pc'

        percentageComplete' :: Text
        percentageComplete' = cs percentageComplete

    katipAddContext (sl "vault" vault) $
     katipAddContext (sl "path"  _path) $
      katipAddContext (sl "partStart" start) $
       katipAddContext (sl "partEnd" end) $
        katipAddContext (sl "percentageComplete" percentageComplete') $ do
         let cr  = contentRange _partStart _partEnd
             ump = Glacier.newUploadMultipartPart accountId vault uploadId cr (cs (p ^. partHash)) body
         $(logTM) InfoS "Uploading part."

         send' env ump

  where

    contentRange :: Int64 -> Int64 -> Text
    contentRange x y = "bytes " `append` cs (show x) `append` accountId `append` cs (show y) `append` "/*"

-- | Complete a multipart upload. All parts must have been successfully uploaded, otherwise
-- this will fail.
completeMulti
    :: (MonadCatch m, MonadIO m, MonadUnliftIO m)
    => AWS.Env                              -- ^ AWS Environment
    -> Text                                 -- ^ Vault
    -> MultiPart                            -- ^ Multipart info.
    -> Glacier.InitiateMultipartUploadResponse      -- ^ Response from when the multipart upload was initiated.
    -> m Glacier.ArchiveCreationOutput
completeMulti env vault mp mu = do
    uploadId <- case mu ^. Glacer.initiateMultipartUploadResponse_uploadId of
                    Nothing     -> throw MissingUploadID
                    Just uid    -> return uid

    let cmu = Glacier.newCompleteMultipartUpload accountId vault uploadId (cs $ show $ mp ^. multipartArchiveSize) (cs $ mp ^. multipartFullHash)

    send' env cmu

-- | Discover credentials in the environment.
newEnv' :: (MonadCatch m, MonadIO m) => m AWS.Env
newEnv' = AWS.newEnv AWS.Discover

-- | Add debug-level log output (handy for seeing
-- all the requests).
setDebug :: (MonadCatch m, MonadIO m) => AWS.Env -> m AWS.Env
setDebug env = do
    lgr <- AWS.newLogger AWS.Debug stdout
    return $ env { AWS._envLogger = lgr } -- FIXME need to set a region here???

-- | Try to run an action, and keep retrying if we catch
-- a 'ServiceError' (e.g. a 408 timeout from Glacier). Don't retry
-- on any other kind of exception.
doWithRetries
    :: (KatipContext m, MonadIO m, MonadCatch m)
    => Int  -- ^ Number of retries.
    -> m a  -- ^ Action to run.
    -> m (Either AWS.Error a)
doWithRetries n action = catch (Right <$> action) f
  where
    f (AWS.ServiceError e)
        | n > 0     = do logServiceError "doWithRetries action failed." $ AWS.ServiceError e
                         doWithRetries (n-1) action

        | otherwise = do $(logTM) ErrorS "Too many errors, giving up."
                         return $ Left $ AWS.ServiceError e

    f e = do $(logTM) ErrorS ("Some other kind of error in doWithRetries, giving up: "
                                <> LogStr (Builder.fromString $ show e))
             return $ Left e

go :: Maybe Int64 -> String -> FilePath -> KatipContextT IO ()
go mPartSizeMb vault' path' = do
    $(logTM) InfoS "Startup."

    let vault = cs vault'

    let myPartSize = fromMaybe 128 mPartSizeMb * oneMb
        archiveDesc = cs path'

    mp  <- liftIO $ mkMultiPart path' myPartSize archiveDesc

    env <- liftIO newEnv'
    mu  <- liftIO $ initiateMulti env vault myPartSize archiveDesc

    let uploadId = fromMaybe (error "No UploadId in response, can't continue multipart upload.")
                 $ mu ^. Glacer.initiateMultipartUploadResponse_uploadId

    let nrParts = length $ mp ^. multiParts

    partResponses <- forM (zip [0..] (mp ^. multiParts)) $ \(i, p) ->
        katipAddContext (sl "uploadId" uploadId) $
        katipAddContext (sl "location" $ fromMaybe "(nothing)" $ mu ^. Glacier.initiateMultipartUploadResponse_location) $
            doWithRetries 10 (uploadOnePart env vault mu p i nrParts)

    case lefts partResponses of
        []   -> do $(logTM) InfoS "All parts uploaded successfully, now completing the multipart upload."
                   catch (do completeResponse <- completeMulti env vault mp mu
                             katipAddContext (sl "uploadId" uploadId)                             $
                              katipAddContext (sl "archiveId" $ completeResponse ^. Glacier.archiveCreationOutput_archiveId) $
                               katipAddContext (sl "checksum" $ completeResponse ^. Glacier.archiveCreationOutput_checksum) $
                                katipAddContext (sl "location" $ completeResponse ^. Glacier.archiveCreationOutput_location) $ do
                                  $(logTM) InfoS "Done"
                                  liftIO exitSuccess)
                         (\e -> do logServiceError "Failed to complete multipart upload." e
                                   $(logTM) ErrorS "Failed."
                                   liftIO exitFailure)

        errs -> do forM_ errs (logServiceError "Failed part upload.")
                   $(logTM) ErrorS "Too many part errors."
                   liftIO exitFailure

-- | Log a 'ServiceError' by pulling out the message and code.
-- We blast through a TransportError or SerializeError as a string
-- because we hate our future self and don't care about structured logging.
logServiceError
    :: (KatipContext m, Monad m)
    => LogStr
    -> AWS.Error
    -> m ()
logServiceError msg (AWS.ServiceError e)
    = let smsg :: Text
          smsg = AWS.toText $ fromMaybe "" $ e ^. AWS.serviceMessage

          scode :: Text
          scode = AWS.toText $ e ^. AWS.serviceCode

        in katipAddContext (sl "serviceMessage" smsg) $
            katipAddContext (sl "serviceCode" scode)  $
             headersAsContext (e ^. AWS.serviceHeaders)   $
               $(logTM) ErrorS msg

logServiceError msg (AWS.TransportError e)
    = let txt :: Text
          txt = AWS.toText $ show e
        in katipAddContext (sl "TransportError" txt) $
            $(logTM) ErrorS msg

logServiceError msg (AWS.SerializeError e)
    = let txt :: Text
          txt = AWS.toText $ show e
        in katipAddContext (sl "SerializeError" txt) $
            $(logTM) ErrorS msg

-- | Turn each header into a context for the structured log.
headersAsContext :: KatipContext m => [AWS.Header] -> m a -> m a
headersAsContext hs = foldl (.) id $ map headerToContext hs
  where
    headerToContext :: KatipContext m => AWS.Header -> m a -> m a
    headerToContext (h, x) = let h' = cs $ CI.original h :: Text
                                 x' = cs x               :: Text
                               in katipAddContext (sl h' x')
