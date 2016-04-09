{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewDataApi.ClientConfigration
   (
      getAccessToken
   )where

import ViewDataApi

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Servant.Client
import Network.HTTP.Client (Manager)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.FilePath.Posix
import System.Directory
import Data.Aeson as J
import Data.Aeson.TH
import GHC.Generics

import Data.Time

import qualified Data.Text    as T
import qualified Data.Text.IO as T

data ClientInfoConfig = ClientInfoConfig{
      clientOxygenInfo :: OxygenClientInfo
   ,  clientOSSBucketInfo :: OSSBucketInfo
}deriving(Eq, Show, Generic)

instance J.FromJSON ClientInfoConfig
instance J.ToJSON ClientInfoConfig

readJSONFromFile :: (FromJSON a) =>  FilePath -> IO (Maybe a)
readJSONFromFile path = do
   exist <- doesFileExist path
   if exist then decodeStrict <$> BS.readFile path
            else return Nothing

getClientConfigFromUserInput :: IO ClientInfoConfig
getClientConfigFromUserInput = do
         putStrLn "Enter your Autodesk Forge Client Id:"
         cid <- T.getLine
         putStrLn "Enter your Autodesk Forge Client Secret:"
         csecret <- T.getLine
         putStrLn "Enter the bucket name you want to upload files to:"
         bn <- getLine
         return $ ClientInfoConfig (OxygenClientInfo cid csecret) (OSSBucketInfo bn Temporary)

-- readClientConfigFromFile :: FilePath -> IO (Maybe ClientInfoConfig)
-- readClientConfigFromFile path = do
--    exist <- doesFileExist path
--    if exist then fmap decodeStrict $ BS.readFile path
--             else return Nothing

getClientInfo :: FilePath -> IO ClientInfoConfig
getClientInfo path = do
   cic <- readJSONFromFile path :: IO (Maybe ClientInfoConfig)
   case cic of Just cfg -> return cfg
               Nothing -> do
                  cfg <- getClientConfigFromUserInput
                  BL.writeFile path $ encode cfg
                  return cfg


isAccessTokenFileFresh :: FilePath -> IO Bool
isAccessTokenFileFresh path = (< 3600.0) <$> (diffUTCTime <$> getCurrentTime <*> getModificationTime path)

readAccessToken :: FilePath -> IO (Maybe OxygenClientToken)
readAccessToken path = do
   token <- readJSONFromFile path :: IO (Maybe OxygenClientToken)
   case token of Just t -> do
                     fresh <- isAccessTokenFileFresh path
                     if fresh then return $ Just t
                              else return Nothing
                 Nothing -> return Nothing

getAccessToken :: FilePath -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO OxygenClientToken
getAccessToken configPath tokenFilePath manager baseURL = do
   clientInfo <- liftIO $ getClientInfo configPath
   token <- liftIO $ readAccessToken tokenFilePath
   case token of Just t -> return t
                 Nothing -> do
                     liftIO $ putStrLn "Fetching token..."
                     st <- getServerAccessToken (clientOxygenInfo clientInfo) manager baseURL
                     liftIO $ BL.writeFile tokenFilePath $ encode st
                     liftIO . putStrLn $ "Token is " ++ access_token st
                     return st
