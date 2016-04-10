{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}


module ViewDataApi.Persistent
   (
      OSSObjectModel(..)
   ,  migrateAll
   ,  insertOrCreateOSSObjectModel
   ,  showAllOSSObjectModels
   )where

import ViewDataApi.CustomPersistentTypes

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Control.Monad.Reader

import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration, SqlPersistT)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase, share, sqlSettings, derivePersistField)

import Text.Printf

import qualified Data.Text    as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OSSObjectModel
   bucketKey String
   objectId String
   objectKey String
   objectSize Int
   objectLocation String
   UniqueObjectId objectId
   conversionStatus ModelConversionStatus default = NotRegistered
   deriving Eq
|]


printObjectSizeToString :: Int -> String
printObjectSizeToString size = sizeString ++ " MB"
                           where mb =  fromIntegral size / (1024.0 *1024.0) :: Float
                                 sizeString = Text.Printf.printf "%.2f" mb :: String

instance Show OSSObjectModel where
   show model = "[" ++ (show . oSSObjectModelConversionStatus) model ++  "] " ++ oSSObjectModelBucketKey model ++ " " ++ oSSObjectModelObjectKey model  ++ " " ++ printObjectSizeToString (oSSObjectModelObjectSize model)

insertOrCreateOSSObjectModel :: OSSObjectModel -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
insertOrCreateOSSObjectModel obj = do
   l <- getBy $ UniqueObjectId $ oSSObjectModelObjectId obj
   case l of Just (Entity ossObjectModelId ossObjectModel) -> replace ossObjectModelId obj
             Nothing -> do
                insert obj
                return ()

showAllOSSObjectModels :: FilePath -> IO ()
showAllOSSObjectModels path = runSqlite (T.pack path) $  do
   models <- selectList [] [] :: SqlPersistT (NoLoggingT (ResourceT IO)) [Entity OSSObjectModel]
   liftIO $ mapM_ (print . entityVal) models
