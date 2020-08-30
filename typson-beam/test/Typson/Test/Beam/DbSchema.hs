{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Typson.Test.Beam.DbSchema
  ( Db(..)
  , db
  , EntityT(..)
  , createTableMigration
  ) where

import qualified Database.Beam as B
import qualified Database.Beam.Postgres as B
import qualified Database.Beam.Migrate as B
import qualified Database.Beam.Backend.SQL.Types as B
import           GHC.Generics (Generic)

import           Typson.Beam (JNullable, nullableJsonb)
import           Typson.Test.Types (Baz)

newtype Db entity
  = Db { _dbEntity :: entity (B.TableEntity EntityT) }
  deriving (Generic, B.Database be)

db :: B.DatabaseSettings be Db
db = B.defaultDbSettings `B.withDbModification`
       B.dbModification
         { _dbEntity = B.setEntityName "beam-entity"
         }

data EntityT f
  = EntityT
    { _entityId :: B.C f (B.SqlSerial Int)
    , _entityGraph :: B.C f (JNullable B.PgJSONB Baz)
    } deriving (Generic, B.Beamable)

instance B.Table EntityT where
  newtype PrimaryKey EntityT f = EntityKey (B.C f (B.SqlSerial Int))
    deriving (Generic, B.Beamable)

  primaryKey = EntityKey . _entityId

tableSchema :: B.Migration B.Postgres (B.CheckedDatabaseEntity B.Postgres db (B.TableEntity EntityT))
tableSchema =
  B.createTable "beam-entity"
    ( EntityT (B.field "id" B.serial B.notNull B.unique)
              (B.field "graph" nullableJsonb B.notNull)
    )

createTableMigration :: B.Migration B.Postgres (Db (B.CheckedDatabaseEntity B.Postgres db))
createTableMigration = Db <$> tableSchema
