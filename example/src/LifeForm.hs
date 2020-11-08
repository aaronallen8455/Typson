{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module LifeForm where

import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Text as T

import           Typson

--------------------------------------------------------------------------------
-- Define example types
--------------------------------------------------------------------------------

data LifeForm =
  LifeForm
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

lifeFormJ :: JsonSchema _ LifeForm
lifeFormJ = object "LifeForm" $
  LifeForm
    <<$> field (key @"classifier") classifier classifierJ
    <<*> field (key @"name") name prim

instance FromJSON LifeForm where
  parseJSON = decodeObject lifeFormJ

instance ToJSON LifeForm where
  toJSON = encodeObject lifeFormJ

plantJ :: JsonSchema _ Plant
plantJ = object "Plant" $
  Plant
    <<$> field (key @"isAquatic") isAquatic prim
    <<*> field (key @"bearsFruit") bearsFruit prim

instance FromJSON Plant where
  parseJSON = decodeObject plantJ

instance ToJSON Plant where
  toJSON = encodeObject plantJ

animalJ :: JsonSchema _ Animal
animalJ = object "Animal" $
  Animal
    <<$> field (key @"favoriteFoods") favoriteFoods (list prim)
    <<*> field (key @"isGoodPet") isGoodPet prim

instance FromJSON Animal where
  parseJSON = decodeObject animalJ

instance ToJSON Animal where
  toJSON = encodeObject animalJ

classifierJ :: JsonSchema _ Classifier
classifierJ = union "Classifier" $
  classifierTags
    <<$> tag (key @"flora") Flora plantJ
    <<*> tag (key @"fauna") Fauna animalJ
  where
    classifierTags withPlant withAnimal = \case
      Flora plant  -> withPlant plant
      Fauna animal -> withAnimal animal

instance FromJSON Classifier where
  parseJSON = decodeObject classifierJ

instance ToJSON Classifier where
  toJSON = encodeObject classifierJ

--------------------------------------------------------------------------------
-- Specimen
--------------------------------------------------------------------------------

lion :: LifeForm
lion =
  LifeForm
    { name = "Lion"
    , classifier = Fauna
        Animal
          { favoriteFoods = ["Zebra", "Antelope", "Giraffe"]
          , isGoodPet = False
          }
    }

dog :: LifeForm
dog =
  LifeForm
    { name = "Dog"
    , classifier = Fauna
        Animal
          { favoriteFoods = ["Chicken", "Peanut Butter", "Salmon"]
          , isGoodPet = True
          }
    }

kelp :: LifeForm
kelp =
  LifeForm
    { name = "Kelp"
    , classifier = Flora
        Plant
          { isAquatic = True
          , bearsFruit = False
          }
    }

bananaTree :: LifeForm
bananaTree =
  LifeForm
    { name = "Banana Tree"
    , classifier = Flora
        Plant
          { isAquatic = False
          , bearsFruit = True
          }
    }
