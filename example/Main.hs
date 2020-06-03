{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Main where

import           Control.Monad.IO.Class (liftIO)
import           Data.Foldable (traverse_)
import qualified Database.Orville.PostgreSQL as O
import qualified Database.Orville.PostgreSQL.Connection as O
import qualified Database.Orville.PostgreSQL.Raw as Raw
import qualified Database.Orville.PostgreSQL.Select as O
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           System.Environment (lookupEnv)

import           Typson (getObjectTree, (:->), (:->>), Idx)
import           Orville (jsonPathSql, JsonSqlParts(..))
import           TestEntity (Entity(..), entityTable, graphField)
import           TestGraph (graphGen, bazJ)

main :: IO ()
main = do
  Just connString <- lookupEnv "CONN_STRING"
  pool <- O.createConnectionPool 1 60 10 connString
  let orvilleEnv = O.newOrvilleEnv pool

  flip O.runOrville orvilleEnv $ do
    O.migrateSchema schema
    liftIO . print =<< runQueries

schema :: O.SchemaDefinition
schema = [O.Table entityTable]

generateData :: O.MonadOrville conn m
             => m ()
generateData = do
  graphs <- Gen.sample (Gen.list (Range.singleton 100) graphGen)

  let mkEntity = Entity ()

  traverse_ (O.insertRecord entityTable) $ mkEntity <$> graphs

runQueries :: O.MonadOrville conn m
           => m [Maybe String]
runQueries = do
  let (JsonSqlParts selector _ fromSql)
        = jsonPathSql @("baz3" `Idx` 1 :->> "foo3") (getObjectTree bazJ) graphField

      sql = "SELECT " <> selector <> " FROM entity"

  Raw.selectSql sql [] fromSql

ids = id : [id]
