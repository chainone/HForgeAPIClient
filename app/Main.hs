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


runCommand :: [String] -> IO ()
runCommand (sub:xs) =
      case sub of "help" -> putStrLn "This is help info"
                  "upload" -> if null xs then putStrLn "No file to upload, please specify the file path after subcommand \"upload \""
                                         else print =<< runExceptT (doUpload $ head xs)
                  "list" -> showAllOSSObjectModels dbFilePath
                  _ -> putStrLn "Unknown sub command"

main :: IO ()
main = getArgs >>= runCommand
