{-# LANGUAGE OverloadedStrings #-}

module Main where

import ViewDataApi

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

oneOxygenClient = OxygenClientInfo "xxx" "xx"
oneOSSBucketInfo = OSSBucketInfo "mybucketwwww" Temporary

main :: IO ()
main = print =<< uselessNumbers

uselessNumbers :: IO (Either ServantError ())
uselessNumbers = runExceptT $ do
   let baseURL = BaseUrl Https "developer.api.autodesk.com" 443 ""
   manager <- liftIO $ newManager tlsManagerSettings
   liftIO $ putStrLn "Fetching token..."
   token <- getToken oneOxygenClient manager baseURL
   liftIO . putStrLn $ "Token is " ++ access_token token
   liftIO . putStrLn $ "Creating OSS bucket..."
   bucket <- createOSSBucket (Just $ tokenHeaderValue token) oneOSSBucketInfo manager baseURL
   liftIO . putStrLn $ "Bucket " ++ bucketKey bucket ++ " created"
