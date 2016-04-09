{-# LANGUAGE OverloadedStrings #-}

module Main where

import ViewDataApi
import ViewDataApi.ClientConfigration

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.FilePath.Posix
import System.Directory
import System.IO.Unsafe
import Data.Aeson as J

import qualified Data.Text    as T
import qualified Data.Text.IO as T

clientInfoFilePath = unsafePerformIO getHomeDirectory </> ".hforge.config"
accessTokenFilePath = unsafePerformIO getHomeDirectory </> ".hforge.token"
baseURL = BaseUrl Https "developer.api.autodesk.com" 443 ""
networkManager = unsafePerformIO $ newManager tlsManagerSettings

main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO (Either ServantError ())
uselessNumbers = runExceptT $ do
   token <- getAccessToken clientInfoFilePath accessTokenFilePath networkManager baseURL
   return ()
   -- liftIO . putStrLn $ "Creating OSS bucket..."
   -- bucket <- createOSSBucket (Just $ tokenHeaderValue token) oneOSSBucketInfo manager baseURL
   -- liftIO . putStrLn $ "Bucket " ++ bucketKey bucket ++ " created"

   -- let fileToUpload = "/Users/xx/first.png"
   -- -- upload a file to oss
   -- liftIO . putStrLn $ "Start to upload a file: " ++ takeFileName fileToUpload
   -- objectInfo <- ossUploadFile token "mybuckex" fileToUpload manager baseURL
   -- liftIO . print $ objectInfo
   -- liftIO . putStrLn $ "Finished Uploading"
