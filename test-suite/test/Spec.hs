{-# LANGUAGE DataKinds, TypeApplications #-}
import           Data.Aeson
import           Hedgehog ((===), forAll, property, tripping)
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range
import           Lens.Micro
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Orville.Spec (orvilleTestTree)
import           Generators (bazGen, fooGen)
import           Types (Baz, Foo, barJ, bazJ, fooJ)
import           Typson

main :: IO ()
main = do
  defaultMain tests

tests :: TestTree
tests =
  testGroup "Typson tests"
    [ testProperty "JSON round-tripping" . property $ do
        x <- forAll bazGen
        tripping x encode eitherDecode

    , testProperty "Lens laws" . property $ do
        foo <- forAll fooGen
        dbl <- forAll $ HH.double (Range.constant (-100) 100)

        let l :: Lens' Foo Double
            l = fieldLens (key @"foo4") fooJ

        -- You get back what you put in
        dbl === set l dbl foo ^. l

        -- Putting back what you got doesn't change anything
        foo === set l (foo ^. l) foo

        -- Setting twice is the same as setting once
        set l dbl (set l dbl foo) === set l dbl foo

    , testProperty "Lens laws with composition" . property $ do
        baz <- forAll bazGen
        dbl <- forAll $ HH.double (Range.constant (-100) 100)

        let l :: Lens' Baz Double
            l = fieldLens (key @"baz1") bazJ
                 . fieldLens (key @"bar4") barJ
                 . fieldLens (key @"foo4") fooJ

        -- You get back what you put in
        dbl === set l dbl baz ^. l

        -- Putting back what you got doesn't change anything
        baz === set l (baz ^. l) baz

        -- Setting twice is the same as setting once
        set l dbl (set l dbl baz) === set l dbl baz

    , orvilleTestTree
    ]

