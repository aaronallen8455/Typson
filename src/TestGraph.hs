{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
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

import           Data.Aeson ((.:), (.:?), FromJSON(..), ToJSON(..), withObject)
import           Data.Function ((&))
import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range

import           JsonTree (JTree, JsonTree(..), Nat(..), Nullability(..), fromValue, toValue, type (.=), type (.==))

data Foo =
  Foo
    { foo1 :: Bool
    , foo2 :: Maybe Int
    , foo3 :: String
    }

fooJ :: JTree Foo _ _
fooJ = Obj Foo "Foo"
     & Prim    @"foo1" foo1
     . OptPrim @"foo2" foo2
     . Prim    @"foo3" foo3

instance ToJSON Foo where
  toJSON = toValue fooJ

instance FromJSON Foo where
  parseJSON = fromValue fooJ

fooGen :: HH.Gen Foo
fooGen =
  Foo
    <$> HH.bool
    <*> HH.maybe (HH.int $ Range.constant 0 100)
    <*> HH.string (Range.constant 1 15) HH.alphaNum

data Bar =
  Bar
    { bar1 :: Foo
    , bar2 :: Maybe Foo
    , bar3 :: Double
    }

barJ :: JTree Bar _ _
barJ = Obj Bar "Bar"
     & SubObj   @"bar1" bar1 fooJ
     . Optional @"bar2" bar2 fooJ
     . Prim     @"bar3" bar3

instance ToJSON Bar where
  toJSON = toValue barJ

instance FromJSON Bar where
  parseJSON = fromValue barJ

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
    , baz3 :: Foo
    }

bazJ :: JTree Baz _ _
bazJ = Obj Baz "Baz"
     & SubObj   @"baz1" baz1 barJ
     . Optional @"baz2" baz2 barJ
     . SubObj   @"baz3" baz3 fooJ

instance ToJSON Baz where
  toJSON = toValue bazJ

instance FromJSON Baz where
  parseJSON = fromValue bazJ

graphGen :: HH.Gen Baz
graphGen =
  Baz
    <$> barGen
    <*> HH.maybe barGen
    <*> fooGen
