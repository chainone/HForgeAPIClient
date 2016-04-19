{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module ViewDataApi.CustomPersistentTypes
   (
      ModelConversionStatus(..)
   )where

import GHC.Generics
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


data ModelConversionStatus = NotRegistered | Registered | Converted
     deriving (Eq, Show, Generic, Read)
derivePersistField "ModelConversionStatus"
