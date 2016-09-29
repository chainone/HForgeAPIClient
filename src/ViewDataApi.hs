{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ViewDataApi
   ( viewDataAPI
   , OxygenClientInfo(..)
   , OxygenClientToken(..)
   , OSSObjectPolicy(..)
   , OSSBucketInfo(..)
   , OSSObjectInfo(..)
   , getServerAccessToken
   , createOSSBucket
   , detailsOSSBucket
   , tokenHeaderValue
   , ossUploadFile
   , registerViewingService
   , checkViewingServiceStatus
   , downloadViewingServiceObjectThumbnail
   ) where

import Control.Applicative as A
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as J
import Data.Aeson.TH
import Data.Monoid
import Data.Proxy
import Data.Typeable
import Data.Text (Text)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BLS
import qualified Network.HTTP.Media as M

import GHC.Generics
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import System.FilePath.Posix
import System.Directory


import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- 1. Auth API

data OxygenClientInfo = OxygenClientInfo{
      oxygenClientId :: Text
   ,  oxygenClientSecret :: Text
} deriving (Eq, Show, Generic)
instance J.FromJSON OxygenClientInfo
instance J.ToJSON OxygenClientInfo

instance ToFormUrlEncoded OxygenClientInfo where
   toFormUrlEncoded clientInfo = [("client_id", oxygenClientId clientInfo), ("client_secret", oxygenClientSecret clientInfo), ("grant_type", "client_credentials")]

data OxygenClientToken = OxygenClientToken{
      token_type :: String
   ,  expires_in :: Int
   ,  access_token :: String
}deriving (Eq, Show, Generic)

instance J.FromJSON OxygenClientToken
instance J.ToJSON OxygenClientToken

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


data Base64OSSObjectURNJSON = Base64OSSObjectURNJSON{
   urn :: String
}deriving (Eq, Show, Generic)
instance J.ToJSON Base64OSSObjectURNJSON


data PNG deriving Typeable


instance MimeUnrender PNG BL.ByteString where
    mimeUnrender _ = Right . id

-- | @Right . toStrict@
instance MimeUnrender PNG BS.ByteString where
    mimeUnrender _ = Right . BL.toStrict



-- We declare what MIME type it represents using the great 'Network.HTTP.Media'
-- package
instance Accept PNG where
    contentType _ = "image" M.// "png"

---------------------------
-- API Declaration
---------------------------
type OxygenAuth = "authentication" :> "v1" :> "authenticate" :> ReqBody '[FormUrlEncoded] OxygenClientInfo :> Post '[JSON] OxygenClientToken
type OSSCreateBucket = "oss" :> "v2" :> "buckets" :> Header "Authorization" String :> ReqBody '[JSON] OSSBucketInfo :> Post '[JSON] OSSBucketInfo
type OSSDetailsBucket = "oss" :> "v2" :> "buckets" :> Capture "bucketKey" String :> "details" :> Header "Authorization" String :> Get '[JSON] J.Object
type OSSUpload = "oss" :> "v2" :> "buckets" :> Capture "bucketKey" String :> "objects" :> Capture "objectName" String :> Header "Authorization" String :> ReqBody '[OctetStream] BL.ByteString :> Put '[JSON] OSSObjectInfo

type RegisterViewingService = "viewingservice" :> "v1" :> "register" :> Header "Authorization" String :> ReqBody '[JSON] Base64OSSObjectURNJSON :> PostNoContent '[JSON] NoContent
type CheckViewingServiceStatus = "viewingservice" :> "v1" :> Capture "base64ObjectURN" String :> "status" :> Header "Authorization" String :> GetNoContent '[JSON] J.Object
type GetViewingServiceObjectThumbnail = "viewingservice" :> "v1" :> "thumbnails" :> Capture "base64ObjectURN" String :> Header "Authorization" String :> Get '[PNG] BL.ByteString

type ViewDataAPI = OxygenAuth :<|> OSSCreateBucket :<|> OSSDetailsBucket :<|> OSSUpload :<|> RegisterViewingService
                              :<|> CheckViewingServiceStatus :<|> GetViewingServiceObjectThumbnail


viewDataAPI :: Proxy ViewDataAPI
viewDataAPI = Proxy

getServerAccessToken ::  OxygenClientInfo -> Manager -> BaseUrl -> ExceptT ServantError IO OxygenClientToken
createOSSBucket ::  Maybe String -> OSSBucketInfo -> Manager -> BaseUrl -> ExceptT ServantError IO OSSBucketInfo
detailsOSSBucket :: String -> Maybe String -> Manager -> BaseUrl -> ExceptT ServantError IO J.Object
ossUpload :: String -> String -> Maybe String -> BL.ByteString -> Manager -> BaseUrl -> ExceptT ServantError IO OSSObjectInfo
registerViewingServiceRaw ::  Maybe String -> Base64OSSObjectURNJSON -> Manager -> BaseUrl -> ExceptT ServantError IO NoContent
checkViewingServiceStatusRaw :: String -> Maybe String -> Manager -> BaseUrl -> ExceptT ServantError IO J.Object
getViewingServiceObjectThumbnailRaw :: String -> Maybe String -> Manager -> BaseUrl -> ExceptT ServantError IO BL.ByteString

getServerAccessToken :<|> createOSSBucket :<|> detailsOSSBucket :<|> ossUpload :<|> registerViewingServiceRaw
                     :<|> checkViewingServiceStatusRaw  :<|> getViewingServiceObjectThumbnailRaw = client viewDataAPI

ossUploadFile :: OxygenClientToken -> String -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO OSSObjectInfo
ossUploadFile token bucketKey filePath manager url = do
   filecontent <- liftIO . BL.readFile $ filePath
   ossUpload bucketKey (takeFileName filePath) (Just $ tokenHeaderValue token) filecontent  manager url


toBase64 :: String -> String
toBase64 = BLS.unpack . encode . BLS.pack

toBase64OSSObjectURNJSON :: String -> Base64OSSObjectURNJSON
toBase64OSSObjectURNJSON  = Base64OSSObjectURNJSON . toBase64

registerViewingService :: OxygenClientToken -> String -> Manager -> BaseUrl -> ExceptT ServantError IO NoContent
registerViewingService token ossURN  = registerViewingServiceRaw (Just $ tokenHeaderValue token) $ toBase64OSSObjectURNJSON ossURN

checkViewingServiceStatus :: OxygenClientToken -> String -> Manager -> BaseUrl -> ExceptT ServantError IO J.Object
checkViewingServiceStatus token ossURN = checkViewingServiceStatusRaw (toBase64 ossURN) (Just $ tokenHeaderValue token)

downloadViewingServiceObjectThumbnail :: OxygenClientToken -> String -> FilePath -> Manager -> BaseUrl -> ExceptT ServantError IO ()
downloadViewingServiceObjectThumbnail token ossURN path manager url = getViewingServiceObjectThumbnailRaw (toBase64 ossURN) (Just $ tokenHeaderValue token) manager url >>= liftIO . BL.writeFile path
