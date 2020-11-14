module Typson.Test.Generators where

import qualified Hedgehog as HH
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range

import           Typson.Test.Types

fooGen :: HH.Gen Foo
fooGen =
  Foo
    <$> HH.list (Range.constant 0 20) HH.bool
    <*> HH.maybe (HH.int $ Range.constant 0 100)
    <*> HH.string (Range.constant 1 15) HH.alphaNum
    <*> HH.double (Range.constant (-1000000) 1000000)
    <*> HH.set (Range.constant 0 10) (HH.int $ Range.constant (-100) 100)

barGen :: HH.Gen Bar
barGen =
  Bar
    <$> fooGen
    <*> HH.maybe fooGen
    <*> HH.double (Range.constant 20 100)
    <*> fooGen
    <*> HH.map (Range.constant 0 3)
               ((,) <$> HH.string (Range.constant 1 20) HH.alphaNum
                    <*> fooGen
               )

bazGen :: HH.Gen Baz
bazGen =
  Baz
    <$> barGen
    <*> HH.maybe barGen
    <*> HH.list (Range.constant 0 5) fooGen
    <*> unionGen

unionGen :: HH.Gen Union
unionGen =
  HH.choice [ U1 <$> HH.bool
            , U2 <$> HH.enumBounded
            , U3 <$> barGen
            ]
