{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
-- Avoids nasty boilerplate
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module TestGraph
  ( Foo(..)
  , fooJ
  , fooGen
  , Bar(..)
  , barJ
  , barGen
  , Baz(..)
  , bazJ
  , graphGen
  ) where

import           Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range

import           Typson ((<<$>), (<<*>), FieldSYM(..), JsonTree, ObjectSYM(..), UnionSYM(..), decodeObject, encodeObject, key)

data Foo =
  Foo
    { foo1 :: [Bool]
    , foo2 :: Maybe Int
    , foo3 :: String
    }

fooJ :: JsonTree _ Foo
fooJ = object "Foo"
     $ Foo
  <<$> field    (key @"foo1") foo1 prim
  <<*> optField (key @"foo2") foo2 prim
  <<*> field    (key @"foo3") foo3 prim

instance ToJSON Foo where
  toJSON = encodeObject fooJ

instance FromJSON Foo where
  parseJSON = decodeObject fooJ

fooGen :: HH.Gen Foo
fooGen =
  Foo
    <$> HH.list (Range.constant 0 20) HH.bool
    <*> HH.maybe (HH.int $ Range.constant 0 100)
    <*> HH.string (Range.constant 1 15) HH.alphaNum

data Bar =
  Bar
    { bar1 :: Foo
    , bar2 :: Maybe Foo
    , bar3 :: Double
    }

barJ :: JsonTree _ Bar
barJ = object "Bar"
     $ Bar
  <<$> field    (key @"bar1") bar1 fooJ
  <<*> optField (key @"bar2") bar2 fooJ
  <<*> field    (key @"bar3") bar3 prim

instance ToJSON Bar where
  toJSON = encodeObject barJ

instance FromJSON Bar where
  parseJSON = decodeObject barJ

barGen :: HH.Gen Bar
barGen =
  Bar
    <$> fooGen
    <*> HH.maybe fooGen
    <*> HH.double (Range.constant 20 100)

data Baz =
  Baz
    { baz1 :: Bar
    , baz2 :: Maybe Bar
    , baz3 :: [Foo]
    }

bazJ :: JsonTree _ Baz
bazJ = object "Baz"
     $ Baz
  <<$> field    (key @"baz1") baz1 barJ
  <<*> optField (key @"baz2") baz2 barJ
  <<*> listField (key @"baz3") baz3 fooJ

instance ToJSON Baz where
  toJSON = encodeObject bazJ

instance FromJSON Baz where
  parseJSON = decodeObject bazJ

graphGen :: HH.Gen Baz
graphGen =
  Baz
    <$> barGen
    <*> HH.maybe barGen
    <*> HH.list (Range.constant 0 5) fooGen

data TestUnion
  = U1 Bool
  | U2 Int
  | U3 Bar

testUnionJ :: (ObjectSYM repr, UnionSYM repr) => repr _ TestUnion
testUnionJ =
  union "TestUnion" $
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

instance ToJSON TestUnion where
  toJSON = encodeObject testUnionJ

instance FromJSON TestUnion where
  parseJSON = decodeObject testUnionJ

