module Typson.Test.Orville.DbSchema
  ( Entity(..)
  , EntityKey
  , entityTable
  , idField
  , graphField
  ) where

import           Data.Int (Int32)

import qualified Database.Orville.PostgreSQL as O

import           Typson.Orville (json)
import           Typson.Test.Types

data Entity key =
  Entity
    { entityId :: key
    , entityGraph :: Baz
    }

type EntityKey = Int32

entityTable :: O.TableDefinition (Entity EntityKey) (Entity ()) EntityKey
entityTable =
  O.mkTableDefinition $
  O.TableParams
    { O.tblName = "orville-entity"
    , O.tblPrimaryKey = idField
    , O.tblMapper =
      Entity
      <$> O.readOnlyField idField
      <*> O.attrField entityGraph graphField
    , O.tblGetKey = entityId
    , O.tblSafeToDelete = []
    , O.tblComments = O.noComments
    }

idField :: O.FieldDefinition EntityKey
idField = O.automaticIdField "id" `O.withFlag` O.PrimaryKey

graphField :: O.FieldDefinition Baz
graphField = O.fieldOfType json "graph"
