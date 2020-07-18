{-# LANGUAGE DataKinds, TypeApplications #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Types where

import           Data.Aeson hiding (object)

import           Typson

data Foo =
  Foo
    { foo1 :: [Bool]
    , foo2 :: Maybe Int
    , foo3 :: String
    , foo4 :: Double
    } deriving (Show, Eq, Ord)

fooJ :: JsonTree _ Foo
fooJ = object "Foo"
     $ Foo
  <<$> prim    (key @"foo1") foo1
  <<*> optPrim (key @"foo2") foo2
  <<*> prim    (key @"foo3") foo3
  <<*> optPrimDef (key @"foo4") foo4 20

defFoo :: Foo
defFoo =
  Foo
    { foo1 = [True]
    , foo2 = Just 5
    , foo3 = "example"
    , foo4 = 812.54
    }

instance ToJSON Foo where
  toJSON = encodeObject fooJ

instance FromJSON Foo where
  parseJSON = decodeObject fooJ

data Bar =
  Bar
    { bar1 :: Foo
    , bar2 :: Maybe Foo
    , bar3 :: Double
    , bar4 :: Foo
    } deriving (Show, Eq, Ord)

barJ :: JsonTree _ Bar
barJ = object "Bar"
     $ Bar
  <<$> subObj    (key @"bar1") bar1 fooJ
  <<*> optSubObj (key @"bar2") bar2 fooJ
  <<*> prim      (key @"bar3") bar3
  <<*> optSubObjDef (key @"bar4") bar4 defFoo fooJ

instance ToJSON Bar where
  toJSON = encodeObject barJ

instance FromJSON Bar where
  parseJSON = decodeObject barJ

data Baz =
  Baz
    { baz1 :: Bar
    , baz2 :: Maybe Bar
    , baz3 :: [Foo]
    } deriving (Show, Eq, Ord)

bazJ :: JsonTree _ Baz
bazJ = object "Baz"
     $ Baz
  <<$> subObj    (key @"baz1") baz1 barJ
  <<*> optSubObj (key @"baz2") baz2 barJ
  <<*> subObjList (key @"baz3") baz3 fooJ

instance ToJSON Baz where
  toJSON = encodeObject bazJ

instance FromJSON Baz where
  parseJSON = decodeObject bazJ

