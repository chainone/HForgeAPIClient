{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewDataApi
   ( viewDataAPI
   , OxygenClientInfo(..)
   , OxygenClientToken(..)
   , OSSObjectPolicy(..)
   , OSSBucketInfo(..)
   , getToken
   , createOSSBucket
   , tokenHeaderValue
   , ossUploadFile
   ) where

import Control.Applicative as A
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as J
import Data.Aeson.TH
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Data.ByteString.Lazy as BL
import System.FilePath.Posix

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- 1. Auth API

data OxygenClientInfo = OxygenClientInfo{
      oxygenClientId :: Text
   ,  oxygenClientSecret :: Text
} deriving (Eq, Show)

instance ToFormUrlEncoded OxygenClientInfo where
   toFormUrlEncoded clientInfo = [("client_id", oxygenClientId clientInfo), ("client_secret", oxygenClientSecret clientInfo), ("grant_type", "client_credentials")]

data OxygenClientToken = OxygenClientToken{
      token_type :: String
   ,  expires_in :: Int
   ,  access_token :: String
}deriving (Eq, Show, Generic)

instance J.FromJSON OxygenClientToken

tokenHeaderValue :: OxygenClientToken -> String
tokenHeaderValue token = token_type token ++ " " ++ access_token token

-- 2. OSS Create bucket
data OSSObjectPolicy = Transient | Temporary | Persistent
   deriving (Eq, Show)

instance J.ToJSON OSSObjectPolicy where
   toJSON Transient = J.String "transient"
   toJSON Temporary = J.String "temporary"
   toJSON Persistent = J.String "persistent"

instance J.FromJSON OSSObjectPolicy where
   parseJSON (J.String "transient") = return Transient
   parseJSON (J.String "temporary") = return Temporary
   parseJSON (J.String "persistent") = return Persistent
   parseJSON _ = A.empty


data OSSBucketInfo = OSSBucketInfo{
      bucketKey :: String
   ,  policyKey :: OSSObjectPolicy
}deriving (Eq, Show, Generic)

instance J.ToJSON OSSBucketInfo

instance J.FromJSON OSSBucketInfo


data OSSObjectInfo = OSSObjectInfo{
      ossBucketKey :: String
   ,  ossObjectId :: String
   ,  ossObjectKey :: String
   ,  ossObjectSize :: Int
   ,  ossObjectLocation :: String
}deriving (Eq, Show, Generic)

ossObjectInfoFieldMapping "ossBucketKey" = "bucketKey"
ossObjectInfoFieldMapping "ossObjectId" = "objectId"
ossObjectInfoFieldMapping "ossObjectKey" = "objectKey"
ossObjectInfoFieldMapping "ossObjectSize" = "size"
ossObjectInfoFieldMapping "ossObjectLocation" = "location"
ossObjectInfoFieldMapping s = s

instance J.FromJSON OSSObjectInfo where
  parseJSON = J.genericParseJSON (J.defaultOptions { fieldLabelModifier = ossObjectInfoFieldMapping })

---------------------------
-- API Declaration
---------------------------
type OxygenAuth = "authentication" :> "v1" :> "authenticate" :> ReqBody '[FormUrlEncoded] OxygenClientInfo :> Post '[JSON] OxygenClientToken
type OSSCreateBucket = "oss" :> "v2" :> "buckets" :> Header "Authorization" String :> ReqBody '[JSON] OSSBucketInfo :> Post '[JSON] OSSBucketInfo
type OSSUpload = "oss" :> "v2" :> "buckets" :> Capture "bucketKey" String :> "objects" :> Capture "objectName" String :> Header "Authorization" String :> ReqBody '[OctetStream] BL.ByteString :> Put '[JSON] OSSObjectInfo

type RegisterViewingService = "viewingservice" :> "v1" :> "register" :> Header "Authorization" String :> ReqBody '[JSON] OSSBucketInfo :> Post '[JSON] OSSBucketInfo
type CheckViewingServiceStatus = "viewingservice" :> "v1" :> Capture "base64ObjectURN" String :> "status" :> Header "Authorization" String :> GetNoContent '[JSON] NoContent
type GetViewingServiceObjectThumbnail = "viewingservice" :> "v1" :> "thumbnails" :> Header "Authorization" String :> Get '[OctetStream] BL.ByteString


type ViewDataAPI = OxygenAuth :<|> OSSCreateBucket :<|> OSSUpload

viewDataAPI :: Proxy ViewDataAPI
viewDataAPI = Proxy

getToken ::  OxygenClientInfo -> Manager -> BaseUrl -> ExceptT ServantError IO OxygenClientToken
createOSSBucket ::  Maybe String -> OSSBucketInfo -> Manager -> BaseUrl -> ExceptT ServantError IO OSSBucketInfo
ossUpload :: String -> String -> Maybe String -> BL.ByteString -> Manager -> BaseUrl -> ExceptT ServantError IO OSSObjectInfo

ossUploadFile :: OxygenClientToken -> String -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO OSSObjectInfo
ossUploadFile token bucketKey filePath manager url = do
   filecontent <- liftIO . BL.readFile $ filePath
   ossUpload bucketKey (takeFileName filePath) (Just $ tokenHeaderValue token) filecontent  manager url

getToken :<|> createOSSBucket :<|> ossUpload = client viewDataAPI
