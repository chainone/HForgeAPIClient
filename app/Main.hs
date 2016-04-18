{-# LANGUAGE OverloadedStrings #-}

module Main where

import ViewDataApi
import ViewDataApi.Persistent
import ViewDataApi.ClientConfigration

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except

import Servant.Client
import Servant.API

import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Data.Maybe
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import qualified Data.Aeson as J
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.FilePath.Posix
import System.Directory
import System.IO.Unsafe
import System.Environment

baseDirectory = unsafePerformIO getHomeDirectory
oxygenClientInfoFilePath = baseDirectory </> ".hforge.config"
accessTokenFilePath = baseDirectory </> ".hforge.token"
bucketFilePath = baseDirectory </> ".hforge.bucket"
dbFilePath = baseDirectory </> ".hforge.sqlite"

baseURL = BaseUrl Https "developer.api.autodesk.com" 443 ""
networkManager = unsafePerformIO $ newManager tlsManagerSettings


doUpload :: FilePath ->  ExceptT ServantError IO OSSObjectModel
doUpload path = do
      info <- liftIO $ getOxygenClientInfo oxygenClientInfoFilePath
      token <- getAccessToken info accessTokenFilePath networkManager baseURL
      liftIO . print $ token
      bucket <- getBucketInfo token bucketFilePath networkManager baseURL
      uploadFile dbFilePath bucket token path networkManager baseURL


doDownload :: String -> FilePath ->  ExceptT ServantError IO ()
doDownload filename dir = do
   info <- liftIO $ getOxygenClientInfo oxygenClientInfoFilePath
   token <- getAccessToken info accessTokenFilePath networkManager baseURL
   liftIO . print $ token
   bucket <- getBucketInfo token bucketFilePath networkManager baseURL
   liftIO $ downloadFileCURL bucket token (dir </> filename)


doGetToken :: ExceptT ServantError IO OxygenClientToken
doGetToken = do
   info <- liftIO $ getOxygenClientInfo oxygenClientInfoFilePath
   getAccessToken info accessTokenFilePath networkManager baseURL

doRegister :: Int -> ExceptT ServantError IO NoContent
doRegister index = do
   token <- doGetToken
   registerStoredOSSObjectModel dbFilePath index token networkManager baseURL

doCheckStatus :: Int -> ExceptT ServantError IO OSSObjectInfo
doCheckStatus index = do
   token <- doGetToken
   checkStoredOSSObjectModelStatus dbFilePath index token networkManager baseURL

runCommand :: [String] -> IO ()
runCommand (sub:xs) =
      case sub of "help" -> putStrLn "This is help info"
                  "upload" -> if null xs then putStrLn "No file to upload, please specify the file path after subcommand \"upload \""
                                         else print =<< runExceptT (doUpload $ head xs)
                  "list" -> showAllOSSObjectModels dbFilePath
                  "download" -> if length xs < 2 then putStrLn "No file to download, please specify the file name and the directory that you want to put the file in after subcommand \"download \""
                                         else print =<< runExceptT (doDownload (head xs) (xs !! 1) )
                  "register" -> runExceptT (doRegister (read (head xs))) >>= print
                  "thumbnail" -> return ()
                  "status" -> runExceptT (doCheckStatus (read (head xs))) >>= print
                  _ -> putStrLn "Unknown sub command"

main :: IO ()
main = getArgs >>= runCommand
