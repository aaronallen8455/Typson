{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Typson.Test.Esqueleto.DbSchema where

import qualified Database.Esqueleto.PostgreSQL.JSON as E
import qualified Database.Persist.TH as P

import           Typson.Test.Types (Baz)

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
EsqueletoEntity
  graph (E.JSONB Baz)
  deriving Show Eq
|]
