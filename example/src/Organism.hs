{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Organism where

import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Text as T

import           Typson

--------------------------------------------------------------------------------
-- Define example types
--------------------------------------------------------------------------------

data Organism =
  Organism
    { classifier :: Classifier
    , name :: T.Text
    } deriving (Eq, Show)

data Classifier
  = Flora Plant
  | Fauna Animal
  deriving (Eq, Show)

data Plant =
  Plant
    { isAquatic :: Bool
    , bearsFruit :: Bool
    } deriving (Eq, Show)

data Animal =
  Animal
    { favoriteFoods :: [T.Text]
    , isGoodPet :: Bool
    } deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Json Tree Definitions
--------------------------------------------------------------------------------

organismJ :: JsonTree _ Organism
organismJ = object "Organism" $
  Organism
    <<$> field (key @"classifier") classifier classifierJ
    <<*> field (key @"name") name prim

instance FromJSON Organism where
  parseJSON = decodeObject organismJ

instance ToJSON Organism where
  toJSON = encodeObject organismJ

plantJ :: JsonTree _ Plant
plantJ = object "Plant" $
  Plant
    <<$> field (key @"isAquatic") isAquatic prim
    <<*> field (key @"bearsFruit") bearsFruit prim

instance FromJSON Plant where
  parseJSON = decodeObject plantJ

instance ToJSON Plant where
  toJSON = encodeObject plantJ

animalJ :: JsonTree _ Animal
animalJ = object "Animal" $
  Animal
    <<$> listField (key @"favoriteFoods") favoriteFoods prim
    <<*> field (key @"isGoodPet") isGoodPet prim

instance FromJSON Animal where
  parseJSON = decodeObject animalJ

instance ToJSON Animal where
  toJSON = encodeObject animalJ

classifierJ :: JsonTree _ Classifier
classifierJ = union "Classifier" $
  handleTags
    <<$> tag (key @"flora") Flora plantJ
    <<*> tag (key @"fauna") Fauna animalJ
  where
    handleTags withPlant withAnimal = \case
      Flora plant  -> withPlant plant
      Fauna animal -> withAnimal animal

instance FromJSON Classifier where
  parseJSON = decodeObject classifierJ

instance ToJSON Classifier where
  toJSON = encodeObject classifierJ

--------------------------------------------------------------------------------
-- Specimen
--------------------------------------------------------------------------------

lion :: Organism
lion =
  Organism
    { name = "Lion"
    , classifier = Fauna
        Animal
          { favoriteFoods = ["Zebra", "Antelope", "Giraffe"]
          , isGoodPet = False
          }
    }

dog :: Organism
dog =
  Organism
    { name = "Dog"
    , classifier = Fauna
        Animal
          { favoriteFoods = ["Chicken", "Peanut Butter", "Salmon"]
          , isGoodPet = True
          }
    }

kelp :: Organism
kelp =
  Organism
    { name = "Kelp"
    , classifier = Flora
        Plant
          { isAquatic = True
          , bearsFruit = False
          }
    }

bananaTree :: Organism
bananaTree =
  Organism
    { name = "Banana Tree"
    , classifier = Flora
        Plant
          { isAquatic = False
          , bearsFruit = True
          }
    }
