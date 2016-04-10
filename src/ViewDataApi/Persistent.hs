{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}


module ViewDataApi.Persistent
   (
      OSSObjectModel(..)
   ,  migrateAll
   ,  insertOrCreateOSSObjectModel
   )where

import Control.Monad.IO.Class  (liftIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration, SqlPersistT)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OSSObjectModel
   bucketKey String
   objectId String
   objectKey String
   objectSize Int
   objectLocation String
   UniqueObjectId objectId
   deriving Eq Show
|]


insertOrCreateOSSObjectModel :: OSSObjectModel -> SqlPersistT (NoLoggingT (ResourceT IO)) ()
insertOrCreateOSSObjectModel obj = do
   l <- getBy $ UniqueObjectId $ oSSObjectModelObjectId obj
   case l of Just (Entity ossObjectModelId ossObjectModel) -> replace ossObjectModelId obj
             Nothing -> do
                insert obj
                return ()
