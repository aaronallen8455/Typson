{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Typson.Optics
-- Description : Interpreting schemas as optics
-- Copyright   : (c) Aaron Allen, 2020
-- Maintainer  : Aaron Allen <aaronallen8455@gmail.com>
-- License     : BSD-style (see the file LICENSE)
-- Stability   : experimental
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Typson.Optics
  ( -- * Optics
    -- | Van-laarhoven style optics derived from schemas.
    fieldLens
  , fieldPrism
  ) where

import           Data.Functor.Identity (Identity(..))
import           Data.Profunctor (Profunctor(dimap))
import           Data.Profunctor.Choice (Choice(..))
import           Data.Kind (Type)
import           Data.Monoid (First(..))
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           GHC.TypeLits (KnownSymbol, Symbol, sameSymbol)
import           Unsafe.Coerce (unsafeCoerce)

import           Typson.JsonTree (Aggregator(..), Edge(..), FieldSYM(..), Multiplicity(..), ObjectSYM(..), Tree(..), UnionSYM(..), runAp, runAp_)
import           Typson.Pathing (TypeAtPath)

--------------------------------------------------------------------------------
-- Derive Optics for Fields
--------------------------------------------------------------------------------

-- | Produce a 'Lens' given a key for an object field and a schema
--
-- >>> edward ^. fieldLens (key @"name") personJ
-- "Edward"
--
fieldLens :: ( KnownSymbol key
             , tree ~ 'Node 'Product edges
             , TypeAtPath obj tree key ~ ty
             )
          => proxy key -- ^ The field's key
          -> Optic key ty tree obj -- ^ The object's schema
          -> Lens' obj ty
fieldLens _ (Lens l) = l

-- | Produce a 'Prism' given a key for a union tag and a schema
--
-- >>> dog ^? fieldLens (key @"classifier") lifeFormJ . fieldPrism (key @"fauna") classifierJ
-- Just (Animal {favoriteFoods = ["Chicken","Peanut Butter","Salmon"], isGoodPet = True})
--
fieldPrism :: ( KnownSymbol key
              , tree ~ 'Node 'Sum edges
              , TypeAtPath obj tree key ~ Maybe ty
              )
           => proxy key -- ^ The tag's key
           -> Optic key ty tree obj -- ^ The union's schema
           -> Prism' obj ty
fieldPrism _ (Prism p) = p

type Lens' s a = forall f. Functor f => (a -> f a) -> s -> f s
type Prism' s a = forall p f. (Choice p, Applicative f) => p a (f a) -> p s (f s)

data Optic (key :: Symbol) (val :: Type) (t :: Tree) (o :: Type) where
  Lens :: t ~ 'Node 'Product es => Lens' o val -> Optic key val t o
  Prism :: t ~ 'Node 'Sum es => Prism' o val -> Optic key val t o
  AbsurdLeaf :: t ~ 'Leaf => Optic key val t o
  AbsurdList :: t ~ 'ListNode st => Optic key val t o

--------------------------------------------------------------------------------
-- Optics implementations
--------------------------------------------------------------------------------

instance KnownSymbol queryKey
    => ObjectSYM (Optic queryKey queryType) where

  object _ fields = Lens $ \afa obj ->
    case getFirst $ runAp_ fGetter fields of
      Nothing -> error "impossible" -- if it type checked, there's guaranteed to be a match
      Just getter ->
        let val = getter obj
            setter o a =
              runIdentity $ runAp (\s -> Identity $ fSetter s a o) fields
         in setter obj <$> afa val

  list _ = AbsurdList

  prim = AbsurdLeaf

instance KnownSymbol queryKey
    => FieldSYM (Optic queryKey queryType) where

  data Field (Optic queryKey queryType) obj tree fieldType =
    Focus { fGetter :: First (obj -> queryType)
          , fSetter :: queryType
                    -> obj
                    -> fieldType
          }

  field :: forall field key subTree tree obj repr proxy edge.
           ( KnownSymbol key
           , edge ~ 'Edge key 'Singleton field subTree
           , tree ~ 'Node 'Product '[edge]
           )
        => proxy key
        -> (obj -> field)
        -> repr subTree field
        -> Field (Optic queryKey queryType) obj tree field
  field _ getter _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, field)) of
      Nothing ->
        Focus
          { fGetter = First Nothing
          , fSetter = \_ obj -> getter obj
          }
      Just Refl ->
        Focus
          { fGetter = First $ Just getter
          , fSetter = const
          }

  optField :: forall field key subTree tree obj repr proxy edge.
              ( KnownSymbol key
              , edge ~ 'Edge key 'Nullable field subTree
              , tree ~ 'Node 'Product '[edge]
              )
           => proxy key
           -> (obj -> Maybe field)
           -> repr subTree field
           -> Field (Optic queryKey queryType) obj tree (Maybe field)
  optField _ getter _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, Maybe field)) of
      Nothing ->
        Focus
          { fGetter = First Nothing
          , fSetter = \_ obj -> getter obj
          }
      Just Refl ->
        Focus
          { fGetter = First $ Just getter
          , fSetter = const
          }

instance KnownSymbol queryKey => UnionSYM (Optic queryKey queryType) where
  type Result (Optic queryKey queryType) union = Maybe queryType

  data Tag (Optic queryKey queryType) union tree vToRes =
    Facet
      { fExtract :: vToRes
      , fEmbed :: First (queryType -> union)
      }

  union _ tags = Prism $ \pafa ->
    case getFirst $ runAp_ fEmbed tags of
      Nothing -> error "impossible" -- if it type checked, there's guaranteed to be a match
      Just embed ->
        dimap f g $ right' pafa
        where
          f u = maybe (Left u) Right
              $ runIdentity (runAp (Identity . fExtract) tags) u
          g = either pure (fmap embed)

  tag :: forall name union v subTree tree proxy edge.
         ( KnownSymbol name
         , edge ~ 'Edge name 'Nullable v subTree
         , tree ~ 'Node 'Sum '[edge]
         )
      => proxy name
      -> (v -> union)
      -> Optic queryKey queryType subTree v
      -> Tag (Optic queryKey queryType) union tree (v -> Maybe queryType)
  tag _ embed _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(name, v)) of
      Nothing ->
        Facet
          { fExtract = const Nothing
          , fEmbed   = First Nothing
          }
      Just Refl ->
        Facet
          { fExtract = Just
          , fEmbed   = First $ Just embed
          }

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- | If the field identifiers are the same, we assume that the field types
-- are also equal, however, this is not enforced by the type system. The
-- TypeAtPath type family is relied upon to enforce this invariant.
sameField :: forall fieldA fieldB typeA typeB.
             (KnownSymbol fieldA, KnownSymbol fieldB)
          => Proxy '(fieldA ,typeA)
          -> Proxy '(fieldB, typeB)
          -> Maybe ('(fieldA, typeA) :~: '(fieldB, typeB))
sameField _ _ =
  unsafeCoerce <$> sameSymbol (Proxy @fieldA) (Proxy @fieldB)

