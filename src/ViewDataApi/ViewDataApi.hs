{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module ViewDataApi
   ( adskAPI
   ) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API
import Servant.Client
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import qualified Data.Text    as T
import qualified Data.Text.IO as T


oneAdskClient = AdskClientInfo "xxx" "xxx"

data AdskClientInfo = AdskClientInfo{
      adskClientId :: Text
   ,  adskClientSecret :: Text
} deriving (Eq, Show)

instance ToFormUrlEncoded AdskClientInfo where
   toFormUrlEncoded clientInfo = [("client_id", adskClientId clientInfo), ("client_secret", adskClientSecret clientInfo), ("grant_type", "client_credentials")]

data AdskClientToken = AdskClientToken{
      token_type :: String
   ,  expires_in :: Int
   ,  access_token :: String
}deriving (Eq, Show, Generic)

instance FromJSON AdskClientToken



type ADSKAPI = "authentication" :> "v1" :> "authenticate" :> ReqBody '[FormUrlEncoded] AdskClientInfo :> Post '[JSON] AdskClientToken
adskAPI :: Proxy ADSKAPI
adskAPI = Proxy



getToken ::  AdskClientInfo -> Manager -> BaseUrl -> ExceptT ServantError IO AdskClientToken

getToken = client adskAPI


-- main :: IO ()
-- main = print =<< uselessNumbers
--
-- uselessNumbers :: IO (Either ServantError ())
-- uselessNumbers = runExceptT $ do
--    let baseURL = BaseUrl Https "developer.api.autodesk.com" 443 ""
--    manager <- liftIO $ newManager tlsManagerSettings
--    token <- getToken oneAdskClient manager baseURL
--    liftIO . putStrLn $ show token
