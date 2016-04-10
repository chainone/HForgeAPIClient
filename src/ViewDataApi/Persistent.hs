{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}

module ViewDataApi.Persistent
   (
      OSSObjectModel(..)
   ,  migrateAll
   )where

import Control.Monad.IO.Class  (liftIO)
import Database.Persist
import Database.Persist.Sqlite (runSqlite, runMigration)
import Database.Persist.TH (mkPersist, mkMigrate, persistLowerCase,
       share, sqlSettings)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
OSSObjectModel
   bucketKey String
   objectId String
   objectKey String
   objectSize Int
   objectLocation String
   deriving Eq Show
|]


-- main :: IO ()
-- main = runSqlite "xxx.sqlite" $ do
--     runMigration migrateAll
--
--     johnId <- insert $ Person "John Doe" $ Just 35
--     janeId <- insert $ Person "Jane Doe" Nothing
--
--     _ <- insert $ BlogPost "My fr1st p0st" johnId
--     _ <- insert $ BlogPost "One more for good measure" johnId
--
--     oneJohnPost <- selectList [BlogPostAuthorId ==. johnId] [LimitTo 1]
--     liftIO $ print (oneJohnPost :: [Entity BlogPost])
--
--     john <- get johnId
--     liftIO $ print (john :: Maybe Person)

    -- delete janeId
    -- deleteWhere [BlogPostAuthorId ==. johnId]
