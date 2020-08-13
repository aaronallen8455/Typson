{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
module Beam.Types
  ( Db(..)
  , db
  , EntityT(..)
  , createTableMigration
  , dropTableMigration
  ) where

import qualified Database.Beam as B
import qualified Database.Beam.Postgres as B
import qualified Database.Beam.Migrate as B
import           GHC.Generics (Generic)

import           Types (Baz)

newtype Db entity
  = Db { _dbEntity :: entity (B.TableEntity EntityT) }
  deriving (Generic, B.Database B.Postgres)

db :: B.DatabaseSettings B.Postgres Db
db = B.defaultDbSettings

data EntityT f
  = EntityT
    { _entityId :: B.C f Int
    , _entityGraph :: B.C f (B.PgJSONB Baz)
    } deriving (Generic, B.Beamable)

instance B.Table EntityT where
  newtype PrimaryKey EntityT f = EntityKey (B.C f Int)
    deriving (Generic, B.Beamable)

  primaryKey = EntityKey . _entityId

tableSchema :: B.Migration B.Postgres (B.CheckedDatabaseEntity B.Postgres db (B.TableEntity EntityT))
tableSchema =
  B.createTable "entity"
    ( EntityT (B.field "id" B.int B.notNull B.unique)
              (B.field "graph" B.jsonb B.notNull)
    )

createTableMigration :: B.Migration B.Postgres (Db (B.CheckedDatabaseEntity B.Postgres db))
createTableMigration = Db <$> tableSchema

dropTableMigration :: B.Migration B.Postgres ()
dropTableMigration = B.dropTable =<< tableSchema
