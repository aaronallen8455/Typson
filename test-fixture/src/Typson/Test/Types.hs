{-# LANGUAGE DataKinds, TypeApplications, TypeOperators #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Typson.Test.Types where

import           Control.Lens
import           Data.Aeson hiding (object)
import           Data.Proxy (Proxy(..))

import           Typson

--------------------------------------------------------------------------------
-- Graph
--------------------------------------------------------------------------------

data Foo =
  Foo
    { foo1 :: [Bool]
    , foo2 :: Maybe Int
    , foo3 :: String
    , foo4 :: Double
    } deriving (Show, Eq, Ord)

fooJ :: JsonSchema _ Foo
fooJ = object "Foo"
     $ Foo
  <<$> field       (key @"foo1") foo1 (list prim)
  <<*> optField    (key @"foo2") foo2 prim
  <<*> field       (key @"foo3") foo3 prim
  <<*> optFieldDef (key @"foo4") foo4 20 prim

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

barJ :: JsonSchema _ Bar
barJ = object "Bar"
     $ Bar
  <<$> field       (key @"bar1") bar1 fooJ
  <<*> optField    (key @"bar2") bar2 fooJ
  <<*> field       (key @"bar3") bar3 prim
  <<*> optFieldDef (key @"bar4") bar4 defFoo fooJ

instance ToJSON Bar where
  toJSON = encodeObject barJ

instance FromJSON Bar where
  parseJSON = decodeObject barJ

data Baz =
  Baz
    { baz1 :: Bar
    , baz2 :: Maybe Bar
    , baz3 :: [Foo]
    , baz4 :: Union
    } deriving (Show, Eq, Ord)

bazJ :: JsonSchema _ Baz
bazJ = object "Baz"
     $ Baz
  <<$> field     (key @"baz1") baz1 barJ
  <<*> optField  (key @"baz2") baz2 barJ
  <<*> field (key @"baz3") baz3 (list fooJ)
  <<*> field     (key @"baz4") baz4 unionJ

instance ToJSON Baz where
  toJSON = encodeObject bazJ

instance FromJSON Baz where
  parseJSON = decodeObject bazJ

data Union
  = U1 Bool
  | U2 Int
  | U3 Bar
  deriving (Show, Eq, Ord)

unionJ :: (ObjectSYM repr, UnionSYM repr) => repr _ Union
unionJ =
  union "Union" $
    tags
      <<$> tag (key @"U1") U1 prim
      <<*> tag (key @"U2") U2 prim
      <<*> tag (key @"U3") U3 barJ
  where
    tags h1 h2 h3 t =
      case t of
        U1 x -> h1 x
        U2 x -> h2 x
        U3 x -> h3 x

instance ToJSON Union where
  toJSON = encodeObject unionJ

instance FromJSON Union where
  parseJSON = decodeObject unionJ

--------------------------------------------------------------------------------
-- Query Paths
--------------------------------------------------------------------------------

basicPath1 :: Proxy ("baz1" :-> "bar3")
basicPath1 = Proxy

basicPath1Getter :: Baz -> Double
basicPath1Getter b =
  b ^. fieldLens (key @"baz1") bazJ
     . fieldLens (key @"bar3") barJ

basicPath2 :: Proxy ("baz1" :-> "bar1" :-> "foo3")
basicPath2 = Proxy

basicPath2Getter :: Baz -> String
basicPath2Getter b =
  b ^. fieldLens (key @"baz1") bazJ
     . fieldLens (key @"bar1") barJ
     . fieldLens (key @"foo3") fooJ

basicPath3 :: Proxy "baz1"
basicPath3 = Proxy

basicPath3Getter :: Baz -> Bar
basicPath3Getter b =
  b ^. fieldLens (key @"baz1") bazJ

optionalPath1 :: Proxy ("baz1" :-> "bar2" :-> "foo4")
optionalPath1 = Proxy

optionalPath1Getter :: Baz -> Maybe Double
optionalPath1Getter b =
  b ^? fieldLens (key @"baz1") bazJ
     . fieldLens (key @"bar2") barJ
     . _Just
     . fieldLens (key @"foo4") fooJ

optionalPath2 :: Proxy ("baz1" :-> "bar2" :-> "foo2")
optionalPath2 = Proxy

optionalPath2Getter :: Baz -> Maybe Int
optionalPath2Getter b =
  b ^? fieldLens (key @"baz1") bazJ
     . fieldLens (key @"bar2") barJ
     . _Just
     . fieldLens (key @"foo2") fooJ
     . _Just

optionalPath3 :: Proxy ("baz2" :-> "bar1" :-> "foo2")
optionalPath3 = Proxy

optionalPath3Getter :: Baz -> Maybe Int
optionalPath3Getter b =
  b ^? fieldLens (key @"baz2") bazJ
     . _Just
     . fieldLens (key @"bar1") barJ
     . fieldLens (key @"foo2") fooJ
     . _Just

listIdxPath1 :: Proxy ("baz1" :-> "bar1" :-> "foo1" :-> 2)
listIdxPath1 = Proxy

listIdxPath1Getter :: Baz -> Maybe Bool
listIdxPath1Getter b =
  b ^? fieldLens (key @"baz1") bazJ
     . fieldLens (key @"bar1") barJ
     . fieldLens (key @"foo1") fooJ
     . ix 2

listIdxPath2 :: Proxy ("baz3" :-> 0 :-> "foo3")
listIdxPath2 = Proxy

listIdxPath2Getter :: Baz -> Maybe String
listIdxPath2Getter b =
  b ^? fieldLens (key @"baz3") bazJ
     . ix 0
     . fieldLens (key @"foo3") fooJ

listIdxPath3 :: Proxy ("baz3" :-> 0 :-> "foo1" :-> 1)
listIdxPath3 = Proxy

listIdxPath3Getter :: Baz -> Maybe Bool
listIdxPath3Getter b =
  b ^? fieldLens (key @"baz3") bazJ
     . ix 0
     . fieldLens (key @"foo1") fooJ
     . ix 1

unionPath1 :: Proxy ("baz4" :-> "U1")
unionPath1 = Proxy

unionPath1Getter :: Baz -> Maybe Bool
unionPath1Getter b =
  b ^? fieldLens (key @"baz4") bazJ
     . fieldPrism (key @"U1") unionJ

unionPath2 :: Proxy ("baz4" :-> "U3" :-> "bar1" :-> "foo1" :-> 0)
unionPath2 = Proxy

unionPath2Getter :: Baz -> Maybe Bool
unionPath2Getter b =
  b ^? fieldLens (key @"baz4") bazJ
     . fieldPrism (key @"U3") unionJ
     . fieldLens (key @"bar1") barJ
     . fieldLens (key @"foo1") fooJ
     . ix 0
