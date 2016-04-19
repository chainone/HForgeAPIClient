{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewDataApi.ClientConfigration
   (  getAccessToken
   ,  getOxygenClientInfo
   ,  getBucketInfo
   ,  uploadFile
   ,  downloadFileCURL
   ,  registerStoredOSSObjectModel
   ,  checkStoredOSSObjectModelStatus
   ,  downloadStoredOSSObjectModel
   ,  downloadStoredOSSObjectModelThumbnail
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

import Servant.API.ContentTypes

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
downloadFileCURL bucket token path = callCommand $ ossDownloadCURLCmd bucket token path

downloadStoredOSSObjectModel :: FilePath -> Int -> FilePath -> OSSBucketInfo -> OxygenClientToken -> Manager -> BaseUrl -> ExceptT ServantError IO ()
downloadStoredOSSObjectModel path index folderPath bucket token manager url = do
      model <- liftIO $ entityVal <$> getStoredOSSObjectModel path index
      liftIO . putStrLn $ "Downloading object " ++ oSSObjectModelObjectKey model
      liftIO $ downloadFileCURL bucket token (folderPath </> oSSObjectModelObjectKey model)


registerStoredOSSObjectModel :: FilePath -> Int -> OxygenClientToken -> Manager -> BaseUrl -> ExceptT ServantError IO NoContent
registerStoredOSSObjectModel path index token manager url = do
      model <- liftIO $ entityVal <$> getStoredOSSObjectModel path index
      liftIO . putStrLn $ "Registering object " ++ oSSObjectModelObjectKey model
      registerViewingService token (oSSObjectModelObjectId model) manager url

checkStoredOSSObjectModelStatus :: FilePath -> Int -> OxygenClientToken -> Manager -> BaseUrl -> ExceptT ServantError IO OSSObjectInfo
checkStoredOSSObjectModelStatus path index token manager url = do
      model <- liftIO $ entityVal <$> getStoredOSSObjectModel path index
      liftIO . putStrLn $ "Checking status for object " ++ oSSObjectModelObjectKey model
      checkViewingServiceStatus token (oSSObjectModelObjectId model) manager url

downloadStoredOSSObjectModelThumbnail :: FilePath -> Int -> FilePath -> OxygenClientToken -> Manager -> BaseUrl -> ExceptT ServantError IO ()
downloadStoredOSSObjectModelThumbnail dbPath index dir token manager url = do
      model <- liftIO $ entityVal <$> getStoredOSSObjectModel dbPath index
      liftIO . putStrLn $ "Downloading thumbnail for object " ++ oSSObjectModelObjectKey model
      let thumbnailPath = addExtension (dropExtension (dir </> oSSObjectModelObjectKey model)) "png"
      downloadViewingServiceObjectThumbnail token (oSSObjectModelObjectId model) thumbnailPath  manager url
      liftIO . putStrLn $ "Thumbnail downloaded to " ++  thumbnailPath
      liftIO . callCommand $ "open \"" ++ thumbnailPath ++ "\""
