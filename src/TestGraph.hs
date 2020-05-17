{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module TestGraph
  ( -- Foo(..)
--  , fooJ
--  , fooGen
--  , Bar(..)
--  , barJ
--  , barGen
--  , Baz(..)
--  , bazJ
--  , graphGen
  ) where

  {-
import           Data.Aeson ((.:), (.:?), FromJSON(..), ToJSON(..), withObject)
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range

import           JsonTree (JsonTree(..), type (.=), type (.==), toValue)

data Foo =
  Foo
    { foo1 :: Bool
    , foo2 :: Maybe Int
    , foo3 :: String
    }

type FooTree =
  '[ "foo1" .== Bool
   , "foo2" .== Maybe Int
   , "foo3" .== String
   ]

fooJ :: JsonTree Foo FooTree
fooJ = Prim @"foo1" foo1
     . Prim @"foo2" foo2
     . Prim @"foo3" foo3
     $ EmptyObj

fooGen :: HH.Gen Foo
fooGen =
  Foo
    <$> HH.bool
    <*> HH.maybe (HH.int $ Range.constant 0 100)
    <*> HH.string (Range.constant 1 15) HH.alphaNum

instance ToJSON Foo where
  toJSON = toValue fooJ

instance FromJSON Foo where
  parseJSON = withObject "Foo" $ \o ->
    Foo
    <$> o .: "foo1"
    <*> o .:? "foo2"
    <*> o .: "foo3"

data Bar =
  Bar
    { bar1 :: Foo
    , bar2 :: Maybe Foo
    , bar3 :: Double
    }

type BarTree =
  '[ "bar1" .= '(Foo, FooTree)
   , "bar2" .= '(Maybe Foo, FooTree)
   , "bar3" .== Double
   ]

barJ :: JsonTree Bar BarTree
barJ = SubObj @"bar1" bar1 fooJ
     . Optional @"bar2" bar2 fooJ
     . Prim @"bar3" bar3
     $ EmptyObj

barGen :: HH.Gen Bar
barGen =
  Bar
    <$> fooGen
    <*> HH.maybe fooGen
    <*> HH.double (Range.constant 20 100)

instance ToJSON Bar where
  toJSON = toValue barJ

instance FromJSON Bar where
  parseJSON = withObject "Bar" $ \o ->
    Bar
    <$> o .: "bar1"
    <*> o .:? "bar2"
    <*> o .: "bar3"

data Baz =
  Baz
    { baz1 :: Bar
    , baz2 :: Maybe Bar
    , baz3 :: Foo
    }

type BazTree =
  '[ "baz1" .= '(Bar, BarTree)
   , "baz2" .= '(Maybe Bar, BarTree)
   , "baz3" .= '(Foo, FooTree)
   ]

bazJ :: JsonTree Baz BazTree
bazJ = SubObj @"baz1" baz1 barJ
     . Optional @"baz2" baz2 barJ
     . SubObj @"baz3" baz3 fooJ
     $ EmptyObj

graphGen :: HH.Gen Baz
graphGen =
  Baz
    <$> barGen
    <*> HH.maybe barGen
    <*> fooGen

instance ToJSON Baz where
  toJSON = toValue bazJ

instance FromJSON Baz where
  parseJSON = withObject "Baz" $ \o ->
    Baz
    <$> o .: "baz1"
    <*> o .:? "baz2"
    <*> o .: "baz3"
    -}
