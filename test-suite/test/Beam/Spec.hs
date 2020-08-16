{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Beam.Spec
  ( beamTestTree
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate as B
import qualified Database.Beam.Postgres as B
import qualified Database.PostgreSQL.Simple as Pg
import           Lens.Micro
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as HH
import           System.Environment (lookupEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Beam.Types (Db(..), EntityT(..), createTableMigration, db)
import           Generators
import           Types
import           Typson
import           Typson.Beam

beamTestTree :: TestTree
beamTestTree = withRunDb $ \runDb ->
  testGroup "Beam Tests"
  [ testCase "JSON Queries" $ do
      graphs <- HH.sample (HH.list (HH.singleton 100) bazGen)
      runDb (insertData graphs)

      r1 <- runDb . B.runSelectReturningList . B.select $
              jsonPath basicPath1 (getObjectTree bazJ) <$> getAllGraphs
      let a1 = flip map graphs $ \g -> JNullable . B.PgJSONB $
                 g ^. fieldLens (key @"baz1") bazJ
                    . fieldLens (key @"bar3") barJ
      assertEqual "Basic Path 1" (sort r1) (sort a1)

      r2 <- runDb . B.runSelectReturningList . B.select $
              jsonPath basicPath2 (getObjectTree bazJ) <$> getAllGraphs
      let a2 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^. fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar1") barJ
               . fieldLens (key @"foo3") fooJ
      assertEqual "Basic Path 2" (sort r2) (sort a2)

      r3 <- runDb . B.runSelectReturningList . B.select $
              jsonPath basicPath3 (getObjectTree bazJ) <$> getAllGraphs
      let a3 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^. fieldLens (key @"baz1") bazJ
      assertEqual "Basic Path 3" (sort r3) (sort a3)

      -- TODO need some way to handle nulls
      r4 <- runDb . B.runSelectReturningList . B.select $
              jsonPath optionalPath1 (getObjectTree bazJ) <$> getAllGraphs
      let a4 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^? fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar2") barJ
               . _Just
               . fieldLens (key @"foo4") fooJ
      assertEqual "Optional Path 1" (sort r4) (sort a4)

      r5 <- runDb . B.runSelectReturningList . B.select $
              jsonPath optionalPath2 (getObjectTree bazJ) <$> getAllGraphs
      let a5 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^? fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar2") barJ
               . _Just
               . fieldLens (key @"foo2") fooJ
               . _Just
      assertEqual "Optional Path 2" (sort r5) (sort a5)

      r6 <- runDb . B.runSelectReturningList . B.select $
              jsonPath optionalPath3 (getObjectTree bazJ) <$> getAllGraphs
      let a6 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^? fieldLens (key @"baz2") bazJ
               . _Just
               . fieldLens (key @"bar1") barJ
               . fieldLens (key @"foo2") fooJ
               . _Just
      assertEqual "Optional Path 3" (sort r6) (sort a6)

      r7 <- runDb . B.runSelectReturningList . B.select $
              jsonPath listIdxPath1 (getObjectTree bazJ) <$> getAllGraphs
      let a7 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^? fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar1") barJ
               . fieldLens (key @"foo1") fooJ
               . ix 2
      assertEqual "List Idx Path 1" (sort r7) (sort a7)

      r8 <- runDb . B.runSelectReturningList . B.select $
              jsonPath listIdxPath2 (getObjectTree bazJ) <$> getAllGraphs
      let a8 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^? fieldLens (key @"baz3") bazJ
               . ix 0
               . fieldLens (key @"foo3") fooJ
      assertEqual "List Idx Path 2" (sort r8) (sort a8)

      r9 <- runDb . B.runSelectReturningList . B.select $
              jsonPath listIdxPath3 (getObjectTree bazJ) <$> getAllGraphs
      let a9 = flip map graphs $ \g -> JNullable . B.PgJSONB $
            g ^? fieldLens (key @"baz3") bazJ
               . ix 0
               . fieldLens (key @"foo1") fooJ
               . ix 1
      assertEqual "List Idx Quer 3" (sort r9) (sort a9)
  ]

getAllGraphs :: B.Q B.Postgres Db s (B.QGenExpr B.QValueContext B.Postgres s (JNullable B.PgJSONB Baz))
getAllGraphs = _entityGraph <$> B.all_ (_dbEntity db)

type DbRunner = forall b. B.Pg b -> IO b

withRunDb :: (DbRunner -> TestTree) -> TestTree
withRunDb mkTree = withDb $ \ioConn -> mkTree $ \action -> do
  conn <- ioConn
  B.runBeamPostgres conn action

withDb :: (IO B.Connection -> TestTree) -> TestTree
withDb = withResource connectToDb B.close

connectToDb :: IO B.Connection
connectToDb = do
  Just connString <- lookupEnv "CONN_STRING"
  conn <- B.connectPostgreSQL (BS.pack connString)

  -- reset the table
  _ <- Pg.execute_ conn "DROP TABLE entity"

  _ <- B.runBeamPostgres conn $
    B.executeMigration B.runNoReturn createTableMigration

  pure conn

insertData :: [Baz] -> B.Pg ()
insertData graphs =
  let mkEntity g = EntityT { _entityId = B.default_
                           , _entityGraph = B.val_ (JNullable $ B.PgJSONB g)
                           }
   in B.runInsert
    . B.insert (_dbEntity db)
    $ B.insertExpressions
    $ map mkEntity graphs
