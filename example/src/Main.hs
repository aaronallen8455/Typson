{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import           Control.Monad (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (LoggingT, runStderrLoggingT)
import qualified Data.ByteString.Char8 as BS
import           Data.Coerce (coerce)
import           Data.Maybe (mapMaybe)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           System.Environment (lookupEnv)

import           Database.Esqueleto
import           Database.Esqueleto.PostgreSQL.JSON
import qualified Database.Persist.Postgresql as P
import qualified Database.Persist.TH as P

import           Typson
import           Typson.Esqueleto
import           Organism

P.share [P.mkPersist P.sqlSettings, P.mkMigrate "migrateAll"] [P.persistLowerCase|
OrganismEntity
  object (JSONB Organism)
  deriving Show Eq
|]

organismEntities :: [OrganismEntity]
organismEntities
  = [ OrganismEntity $ JSONB lion
    , OrganismEntity $ JSONB dog
    , OrganismEntity $ JSONB kelp
    , OrganismEntity $ JSONB bananaTree
    ]

main :: IO ()
main = do
  Just connString <- lookupEnv "CONN_STRING"
  runStderrLoggingT . P.withPostgresqlConn (BS.pack connString) $ \be ->
    (`P.runSqlConn` be) $ do
      P.runMigration migrateAll

      void $ P.insertMany organismEntities

      names <- selectNames
      liftIO $ putStrLn "All Names:"
      liftIO $ print names

      animals <- selectAnimals
      liftIO $ putStrLn "Animals:"
      liftIO $ print animals

      goodPets <- selectGoodPets
      liftIO $ putStrLn "Good Pets:"
      liftIO $ print goodPets

      aquaticPlantNames <- selectAquaticPlantNames
      liftIO $ putStrLn "Aquatic Plants:"
      liftIO $ print aquaticPlantNames

      favoriteFood <- selectFavoriteFood
      liftIO $ putStrLn "Name, Favorite Food:"
      liftIO $ print favoriteFood

selectNames :: P.SqlPersistT (LoggingT IO) [T.Text]
selectNames
  = fmap coerce -- unwrap newtypes
  . select . from
  $ \e ->
      pure . jsonPath (Proxy @("name" :-> ())) organismJ
        $ e ^. OrganismEntityObject

selectAnimals :: P.SqlPersistT (LoggingT IO) [Animal]
selectAnimals
  = fmap (mapMaybe coerce)
  . select . from
  $ \e -> do
      pure . jsonPath (Proxy @("classifier" :->> "fauna")) organismJ
        $ e ^. OrganismEntityObject

selectGoodPets :: P.SqlPersistT (LoggingT IO) [P.Entity OrganismEntity]
selectGoodPets
  = select . from
  $ \e -> do
      let goodPetPath =
            jsonPath (Proxy @("classifier" :-> "fauna" :->> "isGoodPet")) organismJ

      where_ $ goodPetPath (e ^. OrganismEntityObject)
                 ==. val (NullableJSONB (Just True))
      pure e

selectAquaticPlantNames :: P.SqlPersistT (LoggingT IO) [T.Text]
selectAquaticPlantNames
  = fmap coerce -- unwrap newtypes
  . select . from
  $ \e -> do
      let isAquaticPath =
            jsonPath (Proxy @("classifier" :-> "flora" :->> "isAquatic")) organismJ
      where_ $ isAquaticPath (e ^. OrganismEntityObject)
                 ==. val (NullableJSONB (Just True))
      pure $ jsonPath (Proxy @("name" :-> ())) organismJ (e ^. OrganismEntityObject)

selectFavoriteFood :: P.SqlPersistT (LoggingT IO) [(T.Text, Maybe T.Text)]
selectFavoriteFood
  = fmap coerce -- unwrap newtypes
  . select . from
  $ \e -> do
      let o = e ^. OrganismEntityObject
          name' = jsonPath (Proxy @("name" :-> ())) organismJ o
          favoriteFood =
            jsonPath (Proxy @("classifier" :-> "fauna" :->> "favoriteFoods" `Idx` 0)) organismJ o
      pure (name', favoriteFood)
