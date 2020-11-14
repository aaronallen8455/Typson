{-# LANGUAGE DataKinds, TypeApplications #-}
import           Control.Lens
import           Data.Aeson
import           Hedgehog ((===), forAll, property, tripping)
import qualified Hedgehog.Gen as HH
import qualified Hedgehog.Range as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog

import           Typson hiding (set)
import           Typson.Test.Generators (bazGen, fooGen, unionGen)
import           Typson.Test.Types (Baz, Foo, Union, barJ, bazJ, fooJ, unionJ)

main :: IO ()
main = defaultMain tests

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

    , testProperty "Prism laws" . property $ do
        b <- forAll HH.bool

        let u1Prism :: Prism' Union Bool
            u1Prism = fieldPrism (key @"U1") unionJ

        -- preview l (review l b) ≡ Just b
        preview u1Prism (review u1Prism b) === Just b

        s <- forAll unionGen

        -- preview l s ≡ Just a ⟹  review l a ≡ s
        let mbA = preview u1Prism s
        case mbA of
          Just a -> review u1Prism a === s
          Nothing -> pure ()

        -- matching l s ≡ Left t ⟹  matching l t ≡ Left s
        let eT = matching u1Prism s
        case eT of
          Left t -> matching u1Prism t === Left s
          Right _ -> pure ()
    ]

