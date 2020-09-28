{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import           Control.Monad.Catch (handleAll)
import qualified Data.ByteString.Char8 as BS
import           Data.List (sort)
import qualified Database.Selda as S
import qualified Database.Selda.Backend as S
import qualified Database.Selda.PostgreSQL as S
import           Lens.Micro
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
              jsonPath basicPath1 (getObjectTree bazJ) <$> getAllGraphs
      let a1 = flip map graphs $ \g -> Json $
                 g ^. fieldLens (key @"baz1") bazJ
                    . fieldLens (key @"bar3") barJ
      assertEqual "Basic Path 1" (sort r1) (sort a1)

      r2 <- runDb . S.query $
              jsonPath basicPath2 (getObjectTree bazJ) <$> getAllGraphs
      let a2 = flip map graphs $ \g -> Json $
            g ^. fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar1") barJ
               . fieldLens (key @"foo3") fooJ
      assertEqual "Basic Path 2" (sort r2) (sort a2)

      r3 <- runDb . S.query $
              jsonPath basicPath3 (getObjectTree bazJ) <$> getAllGraphs
      let a3 = flip map graphs $ \g -> Json $
            g ^. fieldLens (key @"baz1") bazJ
      assertEqual "Basic Path 3" (sort r3) (sort a3)

      r4 <- runDb . S.query $
              jsonPath optionalPath1 (getObjectTree bazJ) <$> getAllGraphs
      let a4 = flip map graphs $ \g -> Json $
            g ^? fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar2") barJ
               . _Just
               . fieldLens (key @"foo4") fooJ
      assertEqual "Optional Path 1" (sort r4) (sort a4)

      r5 <- runDb . S.query $
              jsonPath optionalPath2 (getObjectTree bazJ) <$> getAllGraphs
      let a5 = flip map graphs $ \g -> Json $
            g ^? fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar2") barJ
               . _Just
               . fieldLens (key @"foo2") fooJ
               . _Just
      assertEqual "Optional Path 2" (sort r5) (sort a5)

      r6 <- runDb . S.query $
              jsonPath optionalPath3 (getObjectTree bazJ) <$> getAllGraphs
      let a6 = flip map graphs $ \g -> Json $
            g ^? fieldLens (key @"baz2") bazJ
               . _Just
               . fieldLens (key @"bar1") barJ
               . fieldLens (key @"foo2") fooJ
               . _Just
      assertEqual "Optional Path 3" (sort r6) (sort a6)

      r7 <- runDb . S.query $
              jsonPath listIdxPath1 (getObjectTree bazJ) <$> getAllGraphs
      let a7 = flip map graphs $ \g -> Json $
            g ^? fieldLens (key @"baz1") bazJ
               . fieldLens (key @"bar1") barJ
               . fieldLens (key @"foo1") fooJ
               . ix 2
      assertEqual "List Idx Path 1" (sort r7) (sort a7)

      r8 <- runDb . S.query $
              jsonPath listIdxPath2 (getObjectTree bazJ) <$> getAllGraphs
      let a8 = flip map graphs $ \g -> Json $
            g ^? fieldLens (key @"baz3") bazJ
               . ix 0
               . fieldLens (key @"foo3") fooJ
      assertEqual "List Idx Path 2" (sort r8) (sort a8)

      r9 <- runDb . S.query $
              jsonPath listIdxPath3 (getObjectTree bazJ) <$> getAllGraphs
      let a9 = flip map graphs $ \g -> Json $
            g ^? fieldLens (key @"baz3") bazJ
               . ix 0
               . fieldLens (key @"foo1") fooJ
               . ix 1
      assertEqual "List Idx Quer 3" (sort r9) (sort a9)
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
