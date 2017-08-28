{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Main where

import Control.Applicative
import Control.Concurrent
import Control.Exception.Safe
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.AWS              (AWST'(..), runAWST)
import Control.Monad.Trans.Resource
import Data.Int
import Data.Either
import Data.Maybe
import Data.String.Conversions              (cs)
import Data.Text                            (append, Text)
import Network.AWS
import Network.AWS.Auth
import Network.AWS.Data
import Network.AWS.Data.Body
import Network.AWS.Glacier.CompleteMultipartUpload
import Network.AWS.Glacier.InitiateMultipartUpload
import Network.AWS.Glacier.UploadArchive
import Network.AWS.Glacier.UploadMultipartPart
import System.Environment
import System.IO
import System.PosixCompat.Files

import qualified Crypto.Hash.SHA256             as SHA256
import qualified Data.ByteString.Base16.Lazy    as B16
import qualified Data.ByteString.Lazy           as BS
import qualified Data.Text.IO                   as Text
import qualified Data.Text.Lazy.Encoding        as TLE

type Hash = BS.ByteString

data Part = Part
    { _partStart    :: Int64
    , _partEnd      :: Int64
    , _partHash     :: Hash
    , _path         :: FilePath
    }
  deriving (Show)

data MultiPart = MultiPart
    { _multipartFullHash    :: Hash
    , _partSize             :: Int64
    , _multiParts           :: [Part]
    , _multipartPath        :: FilePath
    , _multipartArchiveSize :: Int64
    }
  deriving (Show)

makeLenses ''Part
makeLenses ''MultiPart

-- | We use a single '@-@ ' (hyphen) as the account ID so that Amazon Glacier uses the AWS account ID
-- associated with the credentials used to sign the request.
accountId :: Text
accountId = "-"

data MyException = MissingUploadID
    deriving (Show, Typeable)

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

send'
    :: (MonadIO m,
        HasEnv e,
        MonadAWS (AWST' e (ResourceT IO)),
        AWSRequest a)
    => e
    -> Region
    -> a
    -> m (Rs a)
send' env region x = liftIO $ runResourceT . runAWST env . within region $ send x

-- | Upload an archive in one go.
uploadArchiveSingle
    :: (MonadCatch m, MonadIO m)
    => Env
    -> Region
    -> Text
    -> FilePath
    -> m ArchiveCreationOutput
uploadArchiveSingle env region vault f = do
    hash <- treeHash <$> readFile' f
    body <- toHashed <$> readFile' f

    let ua = uploadArchive vault accountId body & uaChecksum .~ Just (cs hash)

    send' env region ua

initiateMulti
    :: (MonadCatch m, MonadIO m)
    => Env
    -> Region
    -> Text
    -> Int64
    -> m InitiateMultipartUploadResponse
initiateMulti env region vault _partSize = send' env region mpu
  where
    mpu = initiateMultipartUpload accountId vault
            & imuPartSize .~ (Just $ cs $ show _partSize)

mkMultiPart
    :: forall m. (MonadCatch m, MonadIO m)
    => FilePath
    -> Int64
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
        _partHash <- treeHash <$> getChunk _multipartPath (_partStart, _partEnd)
        let _path = _multipartPath
        return Part{..}

    f :: Int64 -> Int64 -> [[(Int64, Int64)]]
    f offset remaining
        | remaining <= _partSize = [[(offset, offset + remaining - 1)]]
        | otherwise              = [(offset, offset + _partSize - 1)] : f (offset + _partSize) (remaining - _partSize)

getChunk
    :: (MonadCatch m, MonadIO m)
    => FilePath         -- ^ File to read.
    -> (Int64, Int64)   -- ^ First and last byte to read.
    -> m BS.ByteString
getChunk path (start, end) = f <$> readFile' path
  where
    f = BS.take (end - start + 1) . BS.drop start

uploadOneOfMulti
    :: (MonadCatch m, MonadIO m)
    => Env
    -> Region
    -> Text
    -> InitiateMultipartUploadResponse
    -> Part
    -> m UploadMultipartPartResponse
uploadOneOfMulti env region vault mu p = do
    let Part{..} = p

    body <- toHashed <$> getChunk _path (_partStart, _partEnd)

    uploadId <- case mu ^. imursUploadId of
                    Nothing     -> throw MissingUploadID
                    Just uid    -> return uid

    let cr  = contentRange (p ^. partStart) (p ^. partEnd)

        ump = uploadMultipartPart accountId vault uploadId body
                    & umpChecksum .~ (Just $ cs $ p ^. partHash)
                    & umpRange    .~ Just cr

    send' env region ump

  where

    contentRange :: Int64 -> Int64 -> Text
    contentRange x y = "bytes " `append` cs (show x) `append` accountId `append` cs (show y) `append` "/*"

completeMulti
    :: (MonadCatch m, MonadIO m)
    => Env
    -> Region
    -> Text
    -> MultiPart
    -> InitiateMultipartUploadResponse
    -> m ArchiveCreationOutput
completeMulti env region vault mp mu = do
    uploadId <- case mu ^. imursUploadId of
                    Nothing     -> throw MissingUploadID
                    Just uid    -> return uid

    let cmu = completeMultipartUpload accountId vault uploadId
                    & cmuArchiveSize .~ (Just $ cs $ show $ mp ^. multipartArchiveSize)
                    & cmuChecksum    .~ (Just $ cs $ mp ^. multipartFullHash)

    send' env region cmu

myEnv :: (MonadCatch m, MonadIO m) => m Env
myEnv = do
    env <- newEnv Discover
    lgr <- newLogger Debug stdout
    return $ env & envLogger .~ lgr

doWithRetries n action = do
    putStrLn $ "doWithRetries: " ++ show n
    catch (Right <$> action) f
  where
    f (ServiceError e)
        | n > 0     = do threadDelay $ 5*10^6 -- 5 second sleep
                         doWithRetries (n-1) action
        | otherwise = return $ Left $ ServiceError e

main :: IO ()
main = do
    let region = NorthVirginia

    [vault', path] <- getArgs

    let vault = cs vault'

    let myPartSize = 1024*oneMb

    env <- myEnv
    mu  <- initiateMulti env region vault myPartSize
    print mu

    mp  <- mkMultiPart path myPartSize

    partResponses <- forM (mp ^. multiParts) $ \p -> doWithRetries 10 (uploadOneOfMulti env region vault mu p)
 
    forM_ partResponses print

    case filter isLeft partResponses of
        []   -> do print "All parts uploaded successfully, now completing the multipart upload."
                   completeResponse <- completeMulti env region vault mp mu
                   print completeResponse
        errs -> do print "Errors during upload."
                   forM_ errs print
