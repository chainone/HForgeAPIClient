{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewDataApi.ClientConfigration
   (  getAccessToken
   ,  getOxygenClientInfo
   ,  getBucketInfo
   ,  uploadFile
   ,  downloadFileCURL
   )where

import ViewDataApi
import ViewDataApi.Persistent
import ViewDataApi.CustomPersistentTypes

import Servant.Client

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Data.Time
import Data.Aeson as J
import Data.Aeson.TH
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.FilePath.Posix
import System.Directory
import System.Process

import Network.HTTP.Client (Manager)
import Network.HTTP (urlEncode)
import GHC.Generics

import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)

readJSONFromFile :: (FromJSON a) =>  FilePath -> IO (Maybe a)
readJSONFromFile path = do
   exist <- doesFileExist path
   if exist then decodeStrict <$> BS.readFile path
            else return Nothing

getOxygenClientConfigFromUserInput :: IO OxygenClientInfo
getOxygenClientConfigFromUserInput = do
         putStrLn "Enter your Autodesk Forge Client Id:"
         cid <- T.getLine
         putStrLn "Enter your Autodesk Forge Client Secret:"
         csecret <- T.getLine
         return $ OxygenClientInfo cid csecret

getOxygenClientInfo :: FilePath -> IO OxygenClientInfo
getOxygenClientInfo path = do
   cic <- readJSONFromFile path :: IO (Maybe OxygenClientInfo)
   case cic of Just cfg -> return cfg
               Nothing -> do
                  cfg <- getOxygenClientConfigFromUserInput
                  BL.writeFile path $ encode cfg
                  return cfg

isAccessTokenFileFresh :: FilePath -> IO Bool
isAccessTokenFileFresh path = (< 1200.0) <$> (diffUTCTime <$> getCurrentTime <*> getModificationTime path)

readAccessToken :: FilePath -> IO (Maybe OxygenClientToken)
readAccessToken path = do
   token <- readJSONFromFile path :: IO (Maybe OxygenClientToken)
   case token of Just t -> do
                     fresh <- isAccessTokenFileFresh path
                     if fresh then return $ Just t
                              else return Nothing
                 Nothing -> return Nothing

getAccessToken :: OxygenClientInfo -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO OxygenClientToken
getAccessToken info tokenFilePath manager baseURL = do
   token <- liftIO $ readAccessToken tokenFilePath
   case token of Just t -> return t
                 Nothing -> do
                     liftIO $ putStrLn "Fetching token..."
                     st <- getServerAccessToken info manager baseURL
                     liftIO $ BL.writeFile tokenFilePath $ encode st
                     liftIO . putStrLn $ "Token is " ++ access_token st
                     return st


getBucketInfo :: OxygenClientToken -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO OSSBucketInfo
getBucketInfo token path manager baseURL = do
            bucketInfo <- liftIO (readJSONFromFile path :: IO (Maybe OSSBucketInfo))
            case bucketInfo of Just b -> return b
                               Nothing -> do
                                  liftIO $ putStrLn "Enter the bucket name you want to create:"
                                  name <- liftIO getLine
                                  liftIO $ putStrLn "Creating bucket..."
                                  bkt <- createOSSBucket (Just $ tokenHeaderValue token) (OSSBucketInfo name Temporary) manager baseURL
                                  liftIO $ putStrLn "Bucket created..."
                                  liftIO $ BL.writeFile path $ encode bkt
                                  return bkt

uploadFile :: FilePath -> OSSBucketInfo -> OxygenClientToken -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO OSSObjectModel
uploadFile dbFilePath bInfo token fileToUpload manager baseURL = do
   liftIO . putStrLn $ "Start uploading " ++ takeFileName fileToUpload
   object <- ossUploadFile token (bucketKey bInfo) fileToUpload manager baseURL
   let objectModel = fromServerOSSObject object
   liftIO . runSqlite (T.pack dbFilePath) $ do
      runMigration migrateAll
      insertOrCreateOSSObjectModel objectModel
   return objectModel

fromServerOSSObject :: OSSObjectInfo -> OSSObjectModel
fromServerOSSObject info = OSSObjectModel (ossBucketKey info) (ossObjectId info) (ossObjectKey info) (ossObjectSize info) (ossObjectLocation info) NotRegistered

wrapperString :: String -> String
wrapperString s = "\"" ++ s ++ "\""

ossDownloadCURLCmd :: OSSBucketInfo -> OxygenClientToken -> FilePath -> String
ossDownloadCURLCmd bucket clientToken pathToSave = "curl --header \"Authorization:" ++ tokenHeaderValue clientToken
                                                   ++ "\" -H \"Content-Type:application/octet-stream\" -X GET https://developer.api.autodesk.com/oss/v2/buckets/"
                                                   ++ bucketKey bucket ++ "/objects/" ++ (urlEncode . takeFileName) pathToSave
                                                   ++ " -o " ++ wrapperString pathToSave

downloadFileCURL :: OSSBucketInfo -> OxygenClientToken -> FilePath -> IO ()
downloadFileCURL bucket token path = do
   let cmd = ossDownloadCURLCmd bucket token path
   print cmd
   callCommand cmd
   -- callCommand $ ossDownloadCURLCmd bucket token path
