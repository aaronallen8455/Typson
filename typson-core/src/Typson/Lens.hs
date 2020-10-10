{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
module Typson.Lens
  ( fieldLens
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

import           Typson.JsonTree (FieldSYM(..), Multiplicity(..), Node(..), ObjectSYM(..), Tree, UnionSYM(..), runAp, runAp_)
import           Typson.Pathing (TypeAtPath, (:->))

--------------------------------------------------------------------------------
-- Lens
--------------------------------------------------------------------------------

-- both lens and prism work over the same optic type but contain some constraints
-- based on the tree. the optic type is sum and we just have an impossible case
-- for the absurd branch.

-- why not constrain this to lens preds and have a fieldPrism constrained to prisms
fieldLens :: forall key obj tree ty proxy.
             ( KnownSymbol key
             , TypeAtPath obj tree (key :-> ()) ~ ty
             , GetOpticType (GetMult tree) ~ 'LensOptic
             )
          => proxy key
          -> Optic key ty tree obj
          -> (forall f. Functor f => (ty -> f ty) -> obj -> f obj)
fieldLens _ = \case
  Lens lens -> lens
  Prism _ -> error "impossible"

fieldPrism :: forall key obj tree ty proxy.
              ( KnownSymbol key
              , TypeAtPath obj tree (key :-> ()) ~ ty
              , GetOpticType (GetMult tree) ~ 'PrismOptic
              )
           => proxy key
           -> Optic key ty tree obj
           -> (forall f p. (Choice p, Applicative f) => p ty (f ty) -> p obj (f obj))
fieldPrism _ = \case
  Lens _ -> error "impossible"
  Prism prism -> prism

type family GetMult (t :: Tree) :: Multiplicity where
  GetMult ('Node k mult ty subTree ': rest) = mult

data OpticType = LensOptic | PrismOptic

type family GetOpticType (m :: Multiplicity) :: OpticType where
  GetOpticType 'UnionTag = 'PrismOptic
  GetOpticType other     = 'LensOptic

--------------------------------------------------------------------------------
-- Optic
--------------------------------------------------------------------------------

data Optic (key :: Symbol) (val :: Type) (t :: Tree) (o :: Type)
  = Lens (forall f. Functor f => (val -> f val) -> o -> f o)
  | Prism (forall f p. (Choice p, Applicative f) => p val (f val) -> p o (f o))

instance KnownSymbol queryKey
    => ObjectSYM (Optic queryKey queryType) where

  object _ fields = Lens $ \afa obj ->
    case getFirst $ runAp_ fGetter fields of
      Nothing -> error "the impossible happened!"
      Just getter ->
        let val = getter obj
            setter o a =
              runIdentity $ runAp (\s -> Identity $ fSetter s a o) fields
         in setter obj <$> afa val

  prim = error "impossible"

instance KnownSymbol queryKey
    => FieldSYM (Optic queryKey queryType) where

  data Field (Optic queryKey queryType) obj tree fieldType =
    Focus { fGetter :: First (obj -> queryType)
          , fSetter :: queryType
                    -> obj
                    -> fieldType
          }

  field :: forall field key subTree tree obj repr proxy.
           ( KnownSymbol key
           , KnownSymbol queryKey
           , tree ~ '[ 'Node key 'Singleton field subTree]
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

  optField :: forall field key subTree tree obj repr proxy.
              ( KnownSymbol key
              , KnownSymbol queryKey
              , tree ~ '[ 'Node key 'Nullable field subTree]
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

  listField :: forall field key subTree tree obj repr proxy.
               ( KnownSymbol key
               , KnownSymbol queryKey
               , tree ~ '[ 'Node key 'List field subTree]
               )
            => proxy key
            -> (obj -> [field])
            -> repr subTree field
            -> Field (Optic queryKey queryType) obj tree [field]
  listField _ getter _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, [field])) of
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
      Nothing -> error "impossible"
      Just embed ->
        dimap f g $ right' pafa
        where
          f u = maybe (Left u) Right
              $ runIdentity (runAp (Identity . fExtract) tags) u
          g = either pure (fmap embed)

  tag :: forall name union v subTree tree proxy.
         ( KnownSymbol name
         , tree ~ '[ 'Node name 'UnionTag v subTree]
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

