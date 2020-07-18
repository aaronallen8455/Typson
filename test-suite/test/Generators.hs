module Generators where

import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range

import           Types

fooGen :: HH.Gen Foo
fooGen =
  Foo
    <$> HH.list (Range.constant 0 20) HH.bool
    <*> HH.maybe (HH.int $ Range.constant 0 100)
    <*> HH.string (Range.constant 1 15) HH.alphaNum
    <*> HH.double (Range.constant (-1000000) 1000000)

barGen :: HH.Gen Bar
barGen =
  Bar
    <$> fooGen
    <*> HH.maybe fooGen
    <*> HH.double (Range.constant 20 100)
    <*> fooGen

bazGen :: HH.Gen Baz
bazGen =
  Baz
    <$> barGen
    <*> HH.maybe barGen
    <*> HH.list (Range.constant 0 5) fooGen
