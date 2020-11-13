{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (void)
import           Control.Monad.Catch (handleAll)
import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.PostgreSQL.JSON as E
import qualified Database.Persist.Postgresql as P
import qualified Database.PostgreSQL.Simple as Pg
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as HH
import           System.Environment (lookupEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Typson.Esqueleto
import           Typson.Test.Esqueleto.DbSchema (EsqueletoEntity(..), EntityField(EsqueletoEntityGraph), migrateAll)
import           Typson.Test.Generators
import           Typson.Test.Types

main :: IO ()
main = defaultMain esqueletoTestTree

esqueletoTestTree :: TestTree
esqueletoTestTree = withRunDb $ \runDb ->
  testGroup "Esqueleto Tests"
  [ testCase "JSON Queries" $ do
      graphs <- HH.sample (HH.list (HH.singleton 100) bazGen)
      runDb (insertData graphs)

      r1 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath basicPath1 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a1 = E.Value . NullableJSONB . basicPath1Getter <$> graphs
      assertEqual "Basic Path 1" (sort r1) (sort a1)

      r2 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath basicPath2 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a2 = E.Value . NullableJSONB . basicPath2Getter <$> graphs
      assertEqual "Basic Path 2" (sort r2) (sort a2)

      r3 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath basicPath3 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a3 = E.Value . NullableJSONB . basicPath3Getter <$> graphs
      assertEqual "Basic Path 3" (sort r3) (sort a3)

      r4 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath optionalPath1 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a4 = E.Value . NullableJSONB . optionalPath1Getter <$> graphs
      assertEqual "Optional Path 1" (sort r4) (sort a4)

      r5 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath optionalPath2 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a5 = E.Value . NullableJSONB . optionalPath2Getter <$> graphs
      assertEqual "Optional Path 2" (sort r5) (sort a5)

      r6 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath optionalPath3 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a6 = E.Value . NullableJSONB . optionalPath3Getter <$> graphs
      assertEqual "Optional Path 3" (sort r6) (sort a6)

      r7 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath listIdxPath1 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a7 = E.Value . NullableJSONB . listIdxPath1Getter <$> graphs
      assertEqual "List Idx Path 1" (sort r7) (sort a7)

      r8 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath listIdxPath2 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a8 = E.Value . NullableJSONB . listIdxPath2Getter <$> graphs
      assertEqual "List Idx Path 2" (sort r8) (sort a8)

      r9 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath listIdxPath3 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a9 = E.Value . NullableJSONB . listIdxPath3Getter <$> graphs
      assertEqual "List Idx Path 3" (sort r9) (sort a9)

      r10 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath unionPath1 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a10 = E.Value . NullableJSONB . unionPath1Getter <$> graphs
      assertEqual "Union Path 1" (sort r10) (sort a10)

      r11 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath unionPath2 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a11 = E.Value . NullableJSONB . unionPath2Getter <$> graphs
      assertEqual "Union Path 2" (sort r11) (sort a11)

      r12 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath textMapPath1 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a12 = E.Value . NullableJSONB . textMapPath1Getter <$> graphs
      assertEqual "Text Map Path 1" (sort r12) (sort a12)

      r13 <- runDb . E.select . E.from $ \entity ->
              pure . jsonPath textMapPath2 bazJ
                $ entity E.^. EsqueletoEntityGraph
      let a13 = E.Value . NullableJSONB . textMapPath2Getter <$> graphs
      assertEqual "Text Map Path 2" (sort r13) (sort a13)
  ]

type DbRunner = forall b. P.SqlPersistT IO b -> IO b

withRunDb :: (DbRunner -> TestTree) -> TestTree
withRunDb mkTree = withDb $ \ioBackend -> mkTree $ \action -> do
  backend <- ioBackend
  P.runSqlConn action backend

withDb :: (IO P.SqlBackend -> TestTree) -> TestTree
withDb = withResource connectToDb P.connClose

connectToDb :: IO P.SqlBackend
connectToDb = do
  Just connString <- lookupEnv "CONN_STRING"
  conn <- Pg.connectPostgreSQL $ BS.pack connString
  backend <- P.openSimpleConn (\_ _ _ _ -> pure ()) conn

  -- reset the table
  _ <- handleAll (const $ pure 0) $ Pg.execute_ conn "DROP TABLE \"esqueleto_entity\""

  P.runSqlConn (P.runMigration migrateAll) backend

  pure backend

insertData :: [Baz] -> P.SqlPersistT IO ()
insertData graphs =
  void $ P.insertMany (EsqueletoEntity . E.JSONB <$> graphs)
