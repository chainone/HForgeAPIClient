{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import ViewDataApi
import ViewDataApi.Persistent
import ViewDataApi.ClientConfigration
import ViewDataApi.CustomPersistentTypes

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
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Aeson as J
import qualified Data.Text    as T
import qualified Data.Text.IO as T

import System.FilePath.Posix
import System.Directory
import System.IO.Unsafe
import System.Environment
import System.Process
import Data.FileEmbed
import Data.ByteString.Base64 (encode)

toBase64 :: String -> String
toBase64 = BSC.unpack . encode . BSC.pack

baseDirectory = unsafePerformIO getHomeDirectory

forgeViewingHtmlFile = $(embedFile "forge.viewing.html")
forgeViewingHtmlFilePath = baseDirectory </> "forge.viewing.html"
forgeViewingJSConfigFilePath = baseDirectory </>"forge.viewing.config.js"

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


doDownload :: Int -> FilePath ->  ExceptT ServantError IO ()
doDownload index dir = do
   token <- doGetToken
   bucket <- getBucketInfo token bucketFilePath networkManager baseURL
   downloadStoredOSSObjectModel dbFilePath index dir bucket token networkManager baseURL

doGetToken :: ExceptT ServantError IO OxygenClientToken
doGetToken = do
   info <- liftIO $ getOxygenClientInfo oxygenClientInfoFilePath
   getAccessToken info accessTokenFilePath networkManager baseURL

doRegister :: Int -> ExceptT ServantError IO NoContent
doRegister index = do
   token <- doGetToken
   registerStoredOSSObjectModel dbFilePath index token networkManager baseURL

doCheckStatus :: Int -> ExceptT ServantError IO ModelConversionStatus
doCheckStatus index = do
   token <- doGetToken
   checkStoredOSSObjectModelStatus dbFilePath index token networkManager baseURL


doDownloadThumbnail :: Int -> FilePath -> ExceptT ServantError IO ()
doDownloadThumbnail index dir = do
   token <- doGetToken
   downloadStoredOSSObjectModelThumbnail dbFilePath index dir token networkManager baseURL


generateViewingConfigFileString :: OxygenClientToken -> String -> String
generateViewingConfigFileString token docURN = "var documentURN = 'urn:" ++ docURN ++ "'; var token = \"" ++ access_token token ++ "\";"

generateViewingFiles :: OxygenClientToken -> String -> IO ()
generateViewingFiles token docURN = do
      BS.writeFile forgeViewingHtmlFilePath forgeViewingHtmlFile
      BS.writeFile forgeViewingJSConfigFilePath $ BSC.pack $ generateViewingConfigFileString token docURN


doViewModel :: Int -> ExceptT ServantError IO ()
doViewModel index = do
      token <- doGetToken
      model <- liftIO $ getStoredOSSObjectModelRaw dbFilePath index
      liftIO $ generateViewingFiles token $ (toBase64 . oSSObjectModelObjectId) model
      liftIO $ callCommand $ "open " ++ forgeViewingHtmlFilePath

runCommander :: [String] -> IO ()
runCommander (sub:xs) =
      case sub of "help" -> putStrLn "1. hforge list\n 2 hforge upload file_to_upload\n 3. hforge download 0 ~/\n 4. hforge register 3 5. hforge status 2\n 6. hforge thumbnail 7. hforge view "
                  "upload" -> if null xs then putStrLn "No file to upload, please specify the file path after subcommand \"upload \""
                                         else print =<< runExceptT (doUpload $ head xs)
                  "list" -> showAllOSSObjectModels dbFilePath
                  "download" -> if length xs < 2 then putStrLn "No file to download, please specify the file name and the directory that you want to put the file in after subcommand \"download \""
                                         else print =<< runExceptT (doDownload (read (head xs)) (xs !! 1) )
                  "register" -> runExceptT (doRegister (read (head xs))) >>= print
                  "thumbnail" -> runExceptT (doDownloadThumbnail (read (head xs)) (xs !! 1) ) >>= print
                  "status" -> runExceptT (doCheckStatus (read (head xs))) >>= print
                  "view" -> runExceptT (doViewModel (read (head xs))) >>= print
                  _ -> putStrLn "Unknown sub command"

main :: IO ()
main = getArgs >>= runCommander
