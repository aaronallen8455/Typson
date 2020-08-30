{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
module Typson.Test.Selda.DbSchema where

import qualified Database.Selda as S
import           GHC.Generics (Generic)

import           Typson.Test.Types (Baz)
import           Typson.Selda

data Entity =
  Entity
    { entityId :: S.ID Entity
    , entityGraph :: Json Baz
    } deriving (Show, Generic)

instance S.SqlRow Entity

entityTable :: S.Table Entity
entityTable = S.table "selda-entity" [ #entityId S.:- S.autoPrimary ]
