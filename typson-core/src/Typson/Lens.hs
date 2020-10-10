{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
module Typson.Lens
  ( fieldLens
  , fieldPrism
  ) where

import           Data.Functor.Identity (Identity(..))
import           Data.Profunctor.Choice (Choice(..))
import           Data.Kind (Constraint, Type)
import           Data.Monoid (First(..))
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           GHC.TypeLits (KnownSymbol, Symbol, sameSymbol)
import           Unsafe.Coerce (unsafeCoerce)

import           Typson.JsonTree (FieldSYM(..), IFreeAp, Multiplicity(..), NoDuplicateKeys, Node(..), NonRecursive, ObjectSYM(..), Tree, runAp, runAp_)
import           Typson.Pathing (TypeAtPath, (:->))

--------------------------------------------------------------------------------
-- Lens
--------------------------------------------------------------------------------

-- why not constrain this to lens preds and have a fieldPrism constrained to prisms
fieldLens :: forall key obj tree ty proxy.
             ( KnownSymbol key
             , TypeAtPath obj tree (key :-> ()) ~ ty
             )
          => proxy key
          -> Lens key ty tree obj
          -> (forall f. Functor f => (ty -> f ty) -> obj -> f obj)
fieldLens _ = getOptic

fieldPrism :: forall key obj tree ty proxy.
              ( KnownSymbol key
              , TypeAtPath obj tree (key :-> ()) ~ ty
              )
           => proxy key
           -> Prism key ty tree obj
           -> (forall f p. (Choice p, Applicative f) => p ty (f ty) -> p obj (f obj))
fieldPrism _ = getOptic

--------------------------------------------------------------------------------
-- Optic
--------------------------------------------------------------------------------

-- better to have separate lens and prism types?
type Lens = Optic ((~) (->)) Functor
type Prism = Optic Choice Applicative

newtype Optic (pPred :: (Type -> Type -> Type) -> Constraint)
              (fPred :: (Type -> Type) -> Constraint)
              (key :: Symbol)
              (val :: Type)
              (t :: Tree)
              (o :: Type)
  = Optic
    { getOptic :: forall p f. (pPred p, fPred f)
               => p val (f val)
               -> p o (f o)
    }

instance KnownSymbol queryKey
    => ObjectSYM (Lens queryKey queryType) where

  object :: (NonRecursive '[o] tree, NoDuplicateKeys o tree)
         => String
         -> IFreeAp (Field (Optic ((~) (->)) Functor queryKey queryType) o) tree o
         -> Lens queryKey queryType tree o
  object _ fields = Optic $ \afa obj ->
    case getFirst $ runAp_ fGetter fields of
      Nothing -> error "the impossible happened!"
      Just getter ->
        let val = getter obj
            setter o a =
              runIdentity $ runAp (\s -> Identity $ fSetter s a o) fields
         in setter obj <$> afa val

  prim = error "impossible"

instance KnownSymbol queryKey
    => FieldSYM (Lens queryKey queryType) where

  data Field (Lens queryKey queryType) obj tree fieldType =
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
        -> Field (Lens queryKey queryType) obj tree field
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
           -> Field (Lens queryKey queryType) obj tree (Maybe field)
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
            -> Field (Lens queryKey queryType) obj tree [field]
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

--instance KnownSymbol queryKey => UnionSYM (Prism queryKey queryType) where
--  --type Result repr union :: Type
--  --data Tag repr :: Type -> Tree -> Type -> Type
--  data Tag (Prism queryKey queryType) obj tree fieldType =
--    Facet
--      { fExtract :: obj -> Maybe queryType
--      , fEmbed   :: fieldType -> obj
--      }
--
--  union :: String
--        -> IFreeAp (Tag repr union) tree (union -> Result repr union)
--        -> repr tree union
--  union _ tags = Optic $ \pafa ->
--
--  tag :: ( KnownSymbol name
--         , tree ~ '[ 'Node name 'UnionTag v subTree]
--         )
--      => proxy name
--      -> (v -> union)
--      -> repr subTree v
--      -> Tag repr union tree (v -> Result repr union)


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

