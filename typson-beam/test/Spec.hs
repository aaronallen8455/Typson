{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

import           Control.Monad.Catch (handleAll)
import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)
import qualified Database.Beam as B
import qualified Database.Beam.Migrate as B
import qualified Database.Beam.Postgres as B
import qualified Database.PostgreSQL.Simple as Pg
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as HH
import           System.Environment (lookupEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Typson.Beam
import           Typson.Test.Beam.DbSchema (Db(..), EntityT(..), createTableMigration, db)
import           Typson.Test.Generators
import           Typson.Test.Types

main :: IO ()
main = defaultMain beamTestTree

beamTestTree :: TestTree
beamTestTree = withRunDb $ \runDb ->
  testGroup "Beam Tests"
  [ testCase "JSON Queries" $ do
      graphs <- HH.sample (HH.list (HH.singleton 100) bazGen)
      runDb (insertData graphs)

      r1 <- runDb . B.runSelectReturningList . B.select $
              jsonPath basicPath1 bazJ <$> getAllGraphs
      let a1 = JNullable . B.PgJSONB . basicPath1Getter <$> graphs
      assertEqual "Basic Path 1" (sort r1) (sort a1)

      r2 <- runDb . B.runSelectReturningList . B.select $
              jsonPath basicPath2 bazJ <$> getAllGraphs
      let a2 = JNullable . B.PgJSONB . basicPath2Getter <$> graphs
      assertEqual "Basic Path 2" (sort r2) (sort a2)

      r3 <- runDb . B.runSelectReturningList . B.select $
              jsonPath basicPath3 bazJ <$> getAllGraphs
      let a3 = JNullable . B.PgJSONB . basicPath3Getter <$> graphs
      assertEqual "Basic Path 3" (sort r3) (sort a3)

      r4 <- runDb . B.runSelectReturningList . B.select $
              jsonPath optionalPath1 bazJ <$> getAllGraphs
      let a4 = JNullable . B.PgJSONB . optionalPath1Getter <$> graphs
      assertEqual "Optional Path 1" (sort r4) (sort a4)

      r5 <- runDb . B.runSelectReturningList . B.select $
              jsonPath optionalPath2 bazJ <$> getAllGraphs
      let a5 = JNullable . B.PgJSONB . optionalPath2Getter <$> graphs
      assertEqual "Optional Path 2" (sort r5) (sort a5)

      r6 <- runDb . B.runSelectReturningList . B.select $
              jsonPath optionalPath3 bazJ <$> getAllGraphs
      let a6 = JNullable . B.PgJSONB . optionalPath3Getter <$> graphs
      assertEqual "Optional Path 3" (sort r6) (sort a6)

      r7 <- runDb . B.runSelectReturningList . B.select $
              jsonPath listIdxPath1 bazJ <$> getAllGraphs
      let a7 = JNullable . B.PgJSONB . listIdxPath1Getter <$> graphs
      assertEqual "List Idx Path 1" (sort r7) (sort a7)

      r8 <- runDb . B.runSelectReturningList . B.select $
              jsonPath listIdxPath2 bazJ <$> getAllGraphs
      let a8 = JNullable . B.PgJSONB . listIdxPath2Getter <$> graphs
      assertEqual "List Idx Path 2" (sort r8) (sort a8)

      r9 <- runDb . B.runSelectReturningList . B.select $
              jsonPath listIdxPath3 bazJ <$> getAllGraphs
      let a9 = JNullable . B.PgJSONB . listIdxPath3Getter <$> graphs
      assertEqual "List Idx Path 3" (sort r9) (sort a9)

      r10 <- runDb . B.runSelectReturningList . B.select $
              jsonPath unionPath1 bazJ <$> getAllGraphs
      let a10 = JNullable . B.PgJSONB . unionPath1Getter <$> graphs
      assertEqual "Union Query 1" (sort r10) (sort a10)

      r11 <- runDb . B.runSelectReturningList . B.select $
              jsonPath unionPath2 bazJ <$> getAllGraphs
      let a11 = JNullable . B.PgJSONB . unionPath2Getter <$> graphs
      assertEqual "Union Query 2" (sort r11) (sort a11)
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
  _ <- handleAll (const $ pure 0) $ Pg.execute_ conn "DROP TABLE \"beam-entity\""

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
