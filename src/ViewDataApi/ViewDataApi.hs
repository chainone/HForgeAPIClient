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
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import qualified Data.Aeson as J
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client
import Network.HTTP.Client (Manager)

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
   parseJSON _ = empty


data OSSBucketInfo = OSSBucketInfo{
      bucketKey :: String
   ,  policyKey :: OSSObjectPolicy
}deriving (Eq, Show, Generic)

instance J.ToJSON OSSBucketInfo

instance J.FromJSON OSSBucketInfo

---------------------------
-- API Declaration
---------------------------
type OxygenAuth = "authentication" :> "v1" :> "authenticate" :> ReqBody '[FormUrlEncoded] OxygenClientInfo :> Post '[JSON] OxygenClientToken
type OSSCreateBucket = "oss" :> "v2" :> "buckets" :> Header "Authorization" String :> ReqBody '[JSON] OSSBucketInfo :> Post '[JSON] OSSBucketInfo

type ViewDataAPI = OxygenAuth :<|> OSSCreateBucket

viewDataAPI :: Proxy ViewDataAPI
viewDataAPI = Proxy

getToken ::  OxygenClientInfo -> Manager -> BaseUrl -> ExceptT ServantError IO OxygenClientToken
createOSSBucket ::  Maybe String -> OSSBucketInfo -> Manager -> BaseUrl -> ExceptT ServantError IO OSSBucketInfo

getToken :<|> createOSSBucket = client viewDataAPI
