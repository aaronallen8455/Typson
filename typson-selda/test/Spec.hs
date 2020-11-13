{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import           Control.Monad.Catch (handleAll)
import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)
import qualified Database.Selda as S
import qualified Database.Selda.Backend as S
import qualified Database.Selda.PostgreSQL as S
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as HH
import           System.Environment (lookupEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import           Typson
import           Typson.Selda
import           Typson.Test.Selda.DbSchema
import           Typson.Test.Generators
import           Typson.Test.Types

main :: IO ()
main = defaultMain seldaTestTree

seldaTestTree :: TestTree
seldaTestTree = withRunDb $ \runDb ->
  testGroup "Selda Tests"
  [ testCase "JSON Queries" $ do
      graphs <- HH.sample (HH.list (HH.singleton 100) bazGen)
      runDb (insertData graphs)

      r1 <- runDb . S.query $
              jsonPath basicPath1 bazJ <$> getAllGraphs
      let a1 = Json . basicPath1Getter <$> graphs
      assertEqual "Basic Path 1" (sort r1) (sort a1)

      r2 <- runDb . S.query $
              jsonPath basicPath2 bazJ <$> getAllGraphs
      let a2 = Json . basicPath2Getter <$> graphs
      assertEqual "Basic Path 2" (sort r2) (sort a2)

      r3 <- runDb . S.query $
              jsonPath basicPath3 bazJ <$> getAllGraphs
      let a3 = Json . basicPath3Getter <$> graphs
      assertEqual "Basic Path 3" (sort r3) (sort a3)

      r4 <- runDb . S.query $
              jsonPath optionalPath1 bazJ <$> getAllGraphs
      let a4 = Json . optionalPath1Getter <$> graphs
      assertEqual "Optional Path 1" (sort r4) (sort a4)

      r5 <- runDb . S.query $
              jsonPath optionalPath2 bazJ <$> getAllGraphs
      let a5 = Json . optionalPath2Getter <$> graphs
      assertEqual "Optional Path 2" (sort r5) (sort a5)

      r6 <- runDb . S.query $
              jsonPath optionalPath3 bazJ <$> getAllGraphs
      let a6 = Json . optionalPath3Getter <$> graphs
      assertEqual "Optional Path 3" (sort r6) (sort a6)

      r7 <- runDb . S.query $
              jsonPath listIdxPath1 bazJ <$> getAllGraphs
      let a7 = Json . listIdxPath1Getter <$> graphs
      assertEqual "List Idx Path 1" (sort r7) (sort a7)

      r8 <- runDb . S.query $
              jsonPath listIdxPath2 bazJ <$> getAllGraphs
      let a8 = Json . listIdxPath2Getter <$> graphs
      assertEqual "List Idx Path 2" (sort r8) (sort a8)

      r9 <- runDb . S.query $
              jsonPath listIdxPath3 bazJ <$> getAllGraphs
      let a9 = Json . listIdxPath3Getter <$> graphs
      assertEqual "List Idx Path 3" (sort r9) (sort a9)

      r10 <- runDb . S.query $
              jsonPath unionPath1 bazJ <$> getAllGraphs
      let a10 = Json . unionPath1Getter <$> graphs
      assertEqual "Union Path 1" (sort r10) (sort a10)

      r11 <- runDb . S.query $
              jsonPath unionPath2 bazJ <$> getAllGraphs
      let a11 = Json . unionPath2Getter <$> graphs
      assertEqual "Union Path 2" (sort r11) (sort a11)

      r12 <- runDb . S.query $
              jsonPath textMapPath1 bazJ <$> getAllGraphs
      let a12 = Json . textMapPath1Getter <$> graphs
      assertEqual "Text Map Path 1" (sort r12) (sort a12)

      r13 <- runDb . S.query $
              jsonPath textMapPath2 bazJ <$> getAllGraphs
      let a13 = Json . textMapPath2Getter <$> graphs
      assertEqual "Text Map Path 2" (sort r13) (sort a13)
  ]

getAllGraphs :: S.Query s (S.Col s (Json Baz))
getAllGraphs =
  (S.! #entityGraph) <$> S.select entityTable

type DbRunner = forall b. S.SeldaM S.PG b -> IO b

withRunDb :: (DbRunner -> TestTree) -> TestTree
withRunDb mkTree = withDb $ \ioConn -> mkTree $ \action -> do
  conn <- ioConn
  S.runSeldaT action conn

withDb :: (IO (S.SeldaConnection S.PG) -> TestTree) -> TestTree
withDb = withResource connectToDb S.seldaClose

connectToDb :: IO (S.SeldaConnection S.PG)
connectToDb = do
  Just connString <- lookupEnv "CONN_STRING"
  conn <- S.pgOpen' Nothing (BS.pack connString)

  -- reset the table
  _ <- (`S.runSeldaT` conn) $ do
    handleAll (const $ pure ()) $ S.dropTable entityTable
    S.createTable entityTable

  pure conn

insertData :: [Baz] -> S.SeldaM S.PG ()
insertData graphs =
  let mkEntity g = Entity { entityId = S.def
                          , entityGraph = Json g
                          }

   in S.insert_ entityTable
    $ mkEntity <$> graphs
