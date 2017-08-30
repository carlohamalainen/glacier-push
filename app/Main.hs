{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Main where

import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS              (AWST', runAWST)
import Control.Monad.Trans.Resource
import Data.Int
import Data.Either
import Data.Maybe
import Data.String.Conversions              (cs)
import Data.Text                            (append, Text)
import Katip
import Network.AWS
import Network.AWS.Data
import Network.AWS.Glacier.CompleteMultipartUpload
import Network.AWS.Glacier.InitiateMultipartUpload
import Network.AWS.Glacier.UploadArchive
import Network.AWS.Glacier.UploadMultipartPart
import Network.AWS.Data.Headers
import System.Environment
import System.Exit
import System.IO

import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.ByteString.Base16.Lazy    as B16
import qualified Data.ByteString.Lazy           as BS
import qualified Data.CaseInsensitive           as CI

type Hash = BS.ByteString

-- | Section of a file for a multipart upload. The start
-- and end offsets are inclusive.
data Part = Part
    { _partStart    :: Int64
    , _partEnd      :: Int64
    , _partHash     :: Hash
    , _path         :: FilePath
    }
  deriving (Show)

-- | Things needed for a multipart upload.
data MultiPart = MultiPart
    { _multipartFullHash    :: Hash         -- ^ Hash of the whole file.
    , _partSize             :: Int64        -- ^ Part size in bytes.
    , _multiParts           :: [Part]       -- ^ Description of each part.
    , _multipartPath        :: FilePath     -- ^ Path to the file.
    , _multipartArchiveSize :: Int64        -- ^ Total archive (file) size.
    }
  deriving (Show)

makeLenses ''Part
makeLenses ''MultiPart

-- | We use a single '@-@ ' (hyphen) as the account ID so that Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request.
accountId :: Text
accountId = "-"

data MyException = MissingUploadID deriving (Show, Typeable)

instance Exception MyException

readFile' :: (MonadCatch m, MonadIO m) => String -> m BS.ByteString
readFile' = liftIO . BS.readFile

getFileSize' :: (MonadCatch m, MonadIO m) => FilePath -> m Integer
getFileSize' f = liftIO $ withFile f ReadMode hFileSize

oneMb :: Int64
oneMb = 1024*1024

-- | Compute a tree hash of a bytestring as described in
-- http://docs.aws.amazon.com/amazonglacier/latest/dev/checksum-calculations.html
treeHash :: BS.ByteString -> Hash
treeHash s = treeHash' $ map sha256 $ oneMbChunks s
  where
    treeHash' []  = error "Internal error in treeHash'."
    treeHash' [x] = B16.encode x
    treeHash' xs  = treeHash' $ next xs

    next :: [BS.ByteString] -> [BS.ByteString]
    next []       = []
    next [a]      = [a]
    next (a:b:xs) = sha256 (BS.append a b) : next xs

    oneMbChunks :: BS.ByteString -> [BS.ByteString]
    oneMbChunks x
      | BS.length x <= oneMb = [x]
      | otherwise            = BS.take oneMb x : oneMbChunks (BS.drop oneMb x)

    sha256 :: BS.ByteString -> BS.ByteString
    sha256 = cs . SHA256.hashlazy

-- | Send a request in the AWS monad transformer.
send'
    :: (MonadIO m,
        HasEnv e,
        MonadAWS (AWST' e (ResourceT IO)),
        AWSRequest a)
    => e        -- ^ Environment (e.g. 'Env').
    -> a        -- ^ Request to send, e.g. 'UploadMultipartPart'.
    -> m (Rs a) -- ^ Response.
send' env x = liftIO $ runResourceT . runAWST env $ send x

-- | Upload an archive in one go.
uploadArchiveSingle
    :: (MonadCatch m, MonadIO m)
    => Env                      -- ^ AWS Environment.
    -> Text                     -- ^ Vault name.
    -> FilePath                 -- ^ File to upload.
    -> m ArchiveCreationOutput  -- ^ Response.
uploadArchiveSingle env vault f = do
    hash <- treeHash <$> readFile' f
    body <- toHashed <$> readFile' f

    let ua = uploadArchive vault accountId body
                & uaChecksum .~ Just (cs hash)

    send' env ua

-- | Initiate a multipart upload. We don't need the
-- file name here, just its size.
initiateMulti
    :: (MonadCatch m, MonadIO m)
    => Env                                  -- ^ AWS Environment.
    -> Text                                 -- ^ Vault name.
    -> Int64                                -- ^ Part size (bytes).
    -> m InitiateMultipartUploadResponse    -- ^ Response.
initiateMulti env vault _partSize = send' env mpu
  where
    mpu = initiateMultipartUpload accountId vault
            & imuPartSize .~ (Just $ cs $ show _partSize)

-- | Helper for constructing a 'MultiPart'. Given the part size,
-- this computes the hashes and part boundaries.
mkMultiPart
    :: forall m. (MonadCatch m, MonadIO m)
    => FilePath         -- ^ File.
    -> Int64            -- ^ Part size (bytes).
    -> m MultiPart
mkMultiPart _multipartPath _partSize = do
    _multipartFullHash <- treeHash <$> readFile' _multipartPath

    _multipartArchiveSize <- fromIntegral <$> getFileSize' _multipartPath

    let startEnds :: [(Int64, Int64)]
        startEnds = concat $ f (0::Int64) _multipartArchiveSize

    _multiParts <- forM startEnds mkPart

    return MultiPart{..}

  where

    mkPart :: (Int64, Int64) -> m Part
    mkPart (_partStart, _partEnd) = do
        _partHash <- treeHash <$> getPart _multipartPath (_partStart, _partEnd)
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
getPart path (start, end) = f <$> readFile' path
  where
    f = BS.take (end - start + 1) . BS.drop start

uploadOnePart
    :: (MonadCatch m, MonadIO m)
    => Env                                  -- ^ AWS Environment.
    -> Text                                 -- ^ Vault.
    -> InitiateMultipartUploadResponse      -- ^ Initiated multipart upload.
    -> Part                                 -- ^ A single part.
    -> KatipContextT m UploadMultipartPartResponse
uploadOnePart env vault mu p = do
    let Part{..} = p

    body <- toHashed <$> getPart _path (_partStart, _partEnd)

    uploadId <- case mu ^. imursUploadId of
                    Nothing     -> throw MissingUploadID
                    Just uid    -> return uid

    katipAddContext (sl "partStart" _partStart)
      $ katipAddContext (sl "partEnd" _partEnd)
       $ do let cr  = contentRange _partStart _partEnd

                ump = uploadMultipartPart accountId vault uploadId body
                            & umpChecksum .~ (Just $ cs $ p ^. partHash)
                            & umpRange    .~ Just cr

            $(logTM) InfoS "Uploading part."

            send' env ump

  where

    contentRange :: Int64 -> Int64 -> Text
    contentRange x y = "bytes " `append` cs (show x) `append` accountId `append` cs (show y) `append` "/*"

-- | Complete a multipart upload. All parts must have been successfully uploaded, otherwise
-- this will fail.
completeMulti
    :: (MonadCatch m, MonadIO m)
    => Env                                  -- ^ AWS Environment
    -> Text                                 -- ^ Vault
    -> MultiPart                            -- ^ Multipart info.
    -> InitiateMultipartUploadResponse      -- ^ Response from when the multipart upload was initiated.
    -> m ArchiveCreationOutput
completeMulti env vault mp mu = do
    uploadId <- case mu ^. imursUploadId of
                    Nothing     -> throw MissingUploadID
                    Just uid    -> return uid

    let cmu = completeMultipartUpload accountId vault uploadId
                & cmuArchiveSize .~ (Just $ cs $ show $ mp ^. multipartArchiveSize)
                & cmuChecksum    .~ (Just $ cs $ mp ^. multipartFullHash)

    send' env cmu

-- | Discover credentials in the environment.
newEnv' :: (MonadCatch m, MonadIO m) => m Env
newEnv' = newEnv Discover

-- | Add debug-level log output (handy for seeing
-- all the requests).
setDebug :: (MonadCatch m, MonadIO m) => Env -> m Env
setDebug env = do
    lgr <- newLogger Debug stdout
    return $ env & envLogger .~ lgr

-- | Try to run an action, and keep retrying if we catch
-- a 'ServiceError' (e.g. a 408 timeout from Glacier). Don't retry
-- on any other kind of exception.
doWithRetries
    :: (KatipContext m, MonadIO m, MonadCatch m)
    => Int  -- Number of retries.
    -> m a  -- Action to run.
    -> m (Either Error a)
doWithRetries n action = catch (Right <$> action) f
  where
    f (ServiceError e)
        | n > 0     = do logServiceError "doWithRetries action failed." (ServiceError e)
                         doWithRetries (n-1) action

        | otherwise = do $(logTM) ErrorS "Too many errors, giving up."
                         return $ Left $ ServiceError e

    f e = do $(logTM) ErrorS "Some other kind of error in doWithRetries, giving up."
             return $ Left e

main :: IO ()
main = do
    args <- getArgs

    case args of
        [vault, path] -> main' vault path
        _             -> putStrLn "Usage: glacier-push-exe <vault> <filename>"

  where

    main' :: String -> FilePath -> IO ()
    main' vault path = do
        handleScribe <- mkHandleScribe ColorIfTerminal stdout InfoS V2

        let makeLogEnv = do e <- initLogEnv "glacier-push" "production"
                            registerScribe "stdout" handleScribe defaultScribeSettings e

            initialContext   = ()
            initialNamespace = "main"

        void $ bracket makeLogEnv closeScribes
             $ \le -> runKatipContextT le initialContext initialNamespace
             $ go vault path

-- | Do the needful.
go :: String -> FilePath -> KatipContextT IO ()
go vault' path = do
    $(logTM) InfoS "Startup."

    let vault = cs vault'

    let myPartSize = 128*oneMb

    mp  <- liftIO $ mkMultiPart path myPartSize

    env <- liftIO $ newEnv'
    mu  <- liftIO $ initiateMulti env vault myPartSize

    let uploadId = fromMaybe (error "No UploadId in response, can't continue multipart upload.")
                 $ mu ^. imursUploadId

    partResponses <- forM (mp ^. multiParts) $ \p ->
        katipAddContext (sl "uploadId" uploadId) $
        katipAddContext (sl "location" $ show $ mu ^. imursLocation) $ do
            doWithRetries 10 (uploadOnePart env vault mu p)

    case lefts partResponses of
        []   -> do $(logTM) InfoS "All parts uploaded successfully, now completing the multipart upload."
                   catch (do completeResponse <- completeMulti env vault mp mu
                             katipAddContext (sl "uploadId" uploadId)                             $
                              katipAddContext (sl "archiveId" $ completeResponse ^. acoArchiveId) $
                               katipAddContext (sl "checksum" $ completeResponse ^. acoChecksum ) $
                                katipAddContext (sl "location" $ completeResponse ^. acoLocation) $ do
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
    -> Error
    -> m ()
logServiceError msg (ServiceError e)
    = let smsg :: Text
          smsg = toText $ fromMaybe "" $ e ^. serviceMessage

          scode :: Text
          scode = toText $ e ^. serviceCode

        in katipAddContext (sl "serviceMessage" smsg) $
            katipAddContext (sl "serviceCode" scode)  $
             (headersAsContext $ e ^. serviceHeaders) $
               $(logTM) ErrorS msg

logServiceError msg (TransportError e)
    = let txt :: Text
          txt = toText $ show e
        in katipAddContext (sl "TransportError" txt) $
            $(logTM) ErrorS msg

logServiceError msg (SerializeError e)
    = let txt :: Text
          txt = toText $ show e
        in katipAddContext (sl "SerializeError" txt) $
            $(logTM) ErrorS msg

-- | Turn each header into a context for the structured log.
headersAsContext :: KatipContext m => [Header] -> m a -> m a
headersAsContext hs = foldl (.) id $ map headerToContext hs
  where
    headerToContext :: KatipContext m => Header -> m a -> m a
    headerToContext (h, x) = let h' = cs $ CI.original h :: Text
                                 x' = cs x               :: Text
                               in katipAddContext (sl h' x')
