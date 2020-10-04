{-# LANGUAGE DataKinds, RankNTypes, ScopedTypeVariables #-}

import           Data.Foldable (traverse_)
import           Data.List (sort)
import           Data.Pool (destroyAllResources)
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range
import           System.Environment (lookupEnv)
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Database.Orville.PostgreSQL as O
import qualified Database.Orville.PostgreSQL.Connection as O
import qualified Database.Orville.PostgreSQL.Raw as Raw
import           Typson
import           Typson.Orville (JsonSqlParts(..), jsonPathSql)
import           Typson.Test.Generators (bazGen)
import           Typson.Test.Types
import           Typson.Test.Orville.DbSchema (Entity(..), entityTable, graphField)

main :: IO ()
main = defaultMain orvilleTestTree

orvilleTestTree :: TestTree
orvilleTestTree = withRunDb $ \runDb ->
  testGroup "Orville Tests"
  [ testCase "JSON Queries" $ do
      graphs <- runDb generateData

      r1 <- runDb (runQuery basicQuery1)
      let a1 = map basicPath1Getter graphs
      assertEqual "Basic Query 1" (sort r1) (sort a1)

      r2 <- runDb (runQuery basicQuery2)
      let a2 = map basicPath2Getter graphs
      assertEqual "Basic Query 2" (sort r2) (sort a2)

      r3 <- runDb (runQuery basicQuery3)
      let a3 = map basicPath3Getter graphs
      assertEqual "Basic Query 3" (sort r3) (sort a3)

      r4 <- runDb (runQuery optionalQuery1)
      let a4 = map optionalPath1Getter graphs
      assertEqual "Optional Query 1" (sort r4) (sort a4)

      r5 <- runDb (runQuery optionalQuery2)
      let a5 = map optionalPath2Getter graphs
      assertEqual "Optional Query 2" (sort r5) (sort a5)

      r6 <- runDb (runQuery optionalQuery3)
      let a6 = map optionalPath3Getter graphs
      assertEqual "Optional Query 3" (sort r6) (sort a6)

      r7 <- runDb (runQuery listIdxQuery1)
      let a7 = map listIdxPath1Getter graphs
      assertEqual "List Idx Query 1" (sort r7) (sort a7)

      r8 <- runDb (runQuery listIdxQuery2)
      let a8 = map listIdxPath2Getter graphs
      assertEqual "List Idx Query 2" (sort r8) (sort a8)

      r9 <- runDb (runQuery listIdxQuery3)
      let a9 = map listIdxPath3Getter graphs
      assertEqual "List Idx Quer 3" (sort r9) (sort a9)
  ]

runQuery :: O.MonadOrville conn m => JsonSqlParts a -> m [a]
runQuery (JsonSqlParts selector _ fromSql) =
  let sql = "SELECT " <> selector <> " FROM \"orville-entity\""
   in Raw.selectSql sql [] fromSql

--------------------------------------------------------------------------------
-- Query Types
--------------------------------------------------------------------------------

basicQuery1 :: JsonSqlParts Double
basicQuery1 = jsonPathSql basicPath1 (getObjectTree bazJ) graphField

basicQuery2 :: JsonSqlParts String
basicQuery2 = jsonPathSql basicPath2 (getObjectTree bazJ) graphField

basicQuery3 :: JsonSqlParts Bar
basicQuery3 = jsonPathSql basicPath3 (getObjectTree bazJ) graphField

optionalQuery1 :: JsonSqlParts (Maybe Double)
optionalQuery1 = jsonPathSql optionalPath1 (getObjectTree bazJ) graphField

optionalQuery2 :: JsonSqlParts (Maybe Int)
optionalQuery2 = jsonPathSql optionalPath2 (getObjectTree bazJ) graphField

optionalQuery3 :: JsonSqlParts (Maybe Int)
optionalQuery3 = jsonPathSql optionalPath3 (getObjectTree bazJ) graphField

listIdxQuery1 :: JsonSqlParts (Maybe Bool)
listIdxQuery1 = jsonPathSql listIdxPath1 (getObjectTree bazJ) graphField

listIdxQuery2 :: JsonSqlParts (Maybe String)
listIdxQuery2 = jsonPathSql listIdxPath2 (getObjectTree bazJ) graphField

listIdxQuery3 :: JsonSqlParts (Maybe Bool)
listIdxQuery3 = jsonPathSql listIdxPath3 (getObjectTree bazJ) graphField

--------------------------------------------------------------------------------
-- DB Utils
--------------------------------------------------------------------------------

type DbRunner = forall b. O.OrvilleT O.Connection IO b -> IO b

withRunDb :: (DbRunner -> TestTree)
          -> TestTree
withRunDb mkTree = withDb $ \ioEnv -> mkTree $ \action -> do
  orvilleEnv <- ioEnv
  O.runOrville action orvilleEnv

withDb :: (IO (O.OrvilleEnv O.Connection) -> TestTree) -> TestTree
withDb = withResource acquirePool (destroyAllResources . O.ormEnvPool)

acquirePool :: IO (O.OrvilleEnv O.Connection)
acquirePool = do
  Just connString <- lookupEnv "CONN_STRING"
  pool <- O.createConnectionPool 1 60 10 connString
  let orvilleEnv = O.newOrvilleEnv pool

  O.runOrville resetDb orvilleEnv

  pure orvilleEnv

resetDb :: O.MonadOrville conn m => m ()
resetDb = do
  -- clear the entity table
  O.migrateSchema [O.DropTable "orville-entity"]
  O.migrateSchema [O.Table entityTable]

generateData :: O.MonadOrville conn m
             => m [Baz]
generateData = do
  graphs <- HH.sample (HH.list (Range.singleton 100) bazGen)
  let mkEntity = Entity ()
  traverse_ (O.insertRecord entityTable) $ mkEntity <$> graphs
  pure graphs

