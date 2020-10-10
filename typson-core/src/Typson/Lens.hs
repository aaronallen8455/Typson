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

fieldLens :: forall key obj tree ty proxy pPred fPred mult.
             ( KnownSymbol key
             , TypeAtPath obj tree (key :-> ()) ~ ty
             , GetMult tree ~ mult
             , DerivePreds mult ~ '(pPred, fPred)
             )
          => proxy key
          -> Optic pPred fPred key ty tree obj
          -> (forall p f. (pPred p, fPred f) => p ty (f ty) -> p obj (f obj))
fieldLens _ optic =
  getOptic optic keyProxy
  where
    keyProxy = Proxy @key

type family GetMult (t :: Tree) :: Multiplicity where
  GetMult ('Node key mult ty subTree ': rest) = mult

type family DerivePreds (mult :: Multiplicity) where
  DerivePreds 'UnionTag = '(Choice, Applicative) -- Union types make prisms
  DerivePreds other     = '((~) (->), Functor) -- everything else is a lens

--------------------------------------------------------------------------------
-- Optic
--------------------------------------------------------------------------------

newtype Optic (pPred :: (Type -> Type -> Type) -> Constraint)
              (fPred :: (Type -> Type) -> Constraint)
              (key :: Symbol)
              (val :: Type)
              (t :: Tree)
              (o :: Type)
  = Optic
    { getOptic :: forall p f. (pPred p, fPred f)
               => Proxy key
               -> p val (f val)
               -> p o (f o)
    }

instance KnownSymbol queryKey
    => ObjectSYM (Optic ((~) (->)) Functor queryKey queryType) where

  object :: (NonRecursive '[o] tree, NoDuplicateKeys o tree)
         => String
         -> IFreeAp (Field (Optic ((~) (->)) Functor queryKey queryType) o) tree o
         -> Optic ((~) (->)) Functor queryKey queryType tree o
  object _ fields = Optic $ \keyProxy afa obj ->
    case getFirst $ runAp_ (`fGetter` keyProxy) fields of
      Nothing -> error "the impossible happened!"
      Just getter ->
        let val = getter obj
            setter o a =
              runIdentity $ runAp (\s -> Identity $ fSetter s keyProxy a o) fields
         in setter obj <$> afa val

  prim = error "impossible"

instance forall queryKey queryType. KnownSymbol queryKey
    => FieldSYM (Optic ((~) (->)) Functor queryKey queryType) where

  data Field (Optic ((~) (->)) Functor queryKey queryType) obj tree fieldType =
    Focus { fGetter :: Proxy queryKey
                    -> First (obj -> queryType)
          , fSetter :: Proxy queryKey
                    -> queryType
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
        -> Field (Optic ((~) (->)) Functor queryKey queryType) obj tree field
  field _ getter _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, field)) of
      Nothing ->
        Focus
          { fGetter = \_ -> First Nothing
          , fSetter = \_ _ obj -> getter obj
          }
      Just Refl ->
        Focus
          { fGetter = \_ -> First $ Just getter
          , fSetter = \_ value _ -> value
          }

  optField :: forall field key subTree tree obj repr proxy.
              ( KnownSymbol key
              , KnownSymbol queryKey
              , tree ~ '[ 'Node key 'Nullable field subTree]
              )
           => proxy key
           -> (obj -> Maybe field)
           -> repr subTree field
           -> Field (Optic ((~) (->)) Functor queryKey queryType) obj tree (Maybe field)
  optField _ getter _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, Maybe field)) of
      Nothing ->
        Focus
          { fGetter = \_ -> First Nothing
          , fSetter = \_ _ obj -> getter obj
          }
      Just Refl ->
        Focus
          { fGetter = \_ -> First $ Just getter
          , fSetter = \_ value _ -> value
          }

  listField :: forall field key subTree tree obj repr proxy.
               ( KnownSymbol key
               , KnownSymbol queryKey
               , tree ~ '[ 'Node key 'List field subTree]
               )
            => proxy key
            -> (obj -> [field])
            -> repr subTree field
            -> Field (Optic ((~) (->)) Functor queryKey queryType) obj tree [field]
  listField _ getter _ =
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, [field])) of
      Nothing ->
        Focus
          { fGetter = \_ -> First Nothing
          , fSetter = \_ _ obj -> getter obj
          }
      Just Refl ->
        Focus
          { fGetter = \_ -> First $ Just getter
          , fSetter = \_ value _ -> value
          }

-- instance UnionSYM (Optic Choice Applicative queryKey queryType) where

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

