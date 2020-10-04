{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
module Typson.Lens
  ( fieldLens
  ) where

import           Lens.Micro (Lens', lens)
import           Data.Functor.Identity (Identity(..))
import           Data.Kind (Type)
import           Data.Monoid (First(..))
import           Data.Proxy (Proxy(..))
import           Data.Type.Equality ((:~:)(..))
import           GHC.TypeLits (KnownSymbol, Symbol, sameSymbol)
import           Unsafe.Coerce (unsafeCoerce)

import           Typson.JsonTree (FieldSYM(..), Node(..), ObjectSYM(..), Multiplicity(..), Tree, runAp, runAp_)
import           Typson.Pathing (TypeAtPath, (:->))

--------------------------------------------------------------------------------
-- Lens
--------------------------------------------------------------------------------

fieldLens :: forall key obj tree ty proxy.
             ( KnownSymbol key
             , TypeAtPath obj tree (key :-> ()) ~ ty
             )
          => proxy key
          -> (forall repr. (ObjectSYM repr, FieldSYM repr) => repr tree obj)
          -> Lens' obj ty
fieldLens _ repr =
  lens (runGetter repr keyProxy)
       (runSetter repr keyProxy)
  where
    keyProxy = Proxy @key

--------------------------------------------------------------------------------
-- Getter
--------------------------------------------------------------------------------

newtype Getter (key :: Symbol) (val :: Type) (t :: Tree) o =
  Getter { runGetter :: ( KnownSymbol key
                        , TypeAtPath o t (key :-> ()) ~ val
                        )
                     => Proxy key
                     -> o
                     -> val
         }

instance ObjectSYM (Getter queryKey queryType) where
  object _ ap = Getter $ \keyProxy obj ->
    case getFirst $ runAp_ unGet ap keyProxy of
      Nothing -> error "the impossible happened!"
      Just getter -> getter obj

  prim = error "impossible"

instance forall queryKey queryType. FieldSYM (Getter queryKey queryType) where
  newtype Field (Getter queryKey queryType) obj tree a =
    Get { unGet :: KnownSymbol queryKey => Proxy queryKey -> First (obj -> queryType) }

  field :: forall field key subTree tree obj repr proxy.
           ( KnownSymbol key
           , tree ~ '[ 'Node key 'Singleton field subTree]
           )
        => proxy key
        -> (obj -> field)
        -> repr subTree field
        -> Field (Getter queryKey queryType) obj tree field
  field _ getter _ = Get $ \_ -> First $
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, field)) of
      Nothing -> Nothing
      Just Refl -> Just getter

  optField :: forall field key subTree tree obj repr proxy.
              ( KnownSymbol key
              , tree ~ '[ 'Node key 'Nullable field subTree]
              )
           => proxy key
           -> (obj -> Maybe field)
           -> repr subTree field
           -> Field (Getter queryKey queryType) obj tree (Maybe field)
  optField _ getter _ = Get $ \_ -> First $
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, Maybe field)) of
      Nothing -> Nothing
      Just Refl -> Just getter

  listField :: forall field key subTree tree obj repr proxy.
               ( KnownSymbol key
               , tree ~ '[ 'Node key 'List field subTree]
               )
            => proxy key
            -> (obj -> [field])
            -> repr subTree field
            -> Field (Getter queryKey queryType) obj tree [field]
  listField _ getter _ = Get $ \_ -> First $
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, [field])) of
      Nothing -> Nothing
      Just Refl -> Just getter

--------------------------------------------------------------------------------
-- Setter
--------------------------------------------------------------------------------

newtype Setter (key :: Symbol) (fieldType :: Type) (t :: Tree) o =
  Setter { runSetter :: ( KnownSymbol key
                        , TypeAtPath o t (key :-> ()) ~ fieldType
                        )
                     => Proxy key
                     -> o
                     -> fieldType
                     -> o
         }

instance ObjectSYM (Setter queryKey queryType) where
  object _ ap = Setter $ \keyProxy obj val ->
    runIdentity $ runAp (\s -> Identity $ runSet s keyProxy val obj) ap
  prim = Setter $ \_ o _ -> o

instance forall queryKey queryType. FieldSYM (Setter queryKey queryType) where
  newtype Field (Setter queryKey queryType) obj tree fieldType =
    Set { runSet :: KnownSymbol queryKey
                 => Proxy queryKey
                 -> queryType
                 -> obj
                 -> fieldType
        }

  field :: forall field key subTree tree obj repr proxy.
           ( KnownSymbol key
           , tree ~ '[ 'Node key 'Singleton field subTree]
           )
        => proxy key
        -> (obj -> field)
        -> repr subTree field
        -> Field (Setter queryKey queryType) obj tree field
  field _ getter _ = Set $ \_ value obj ->
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, field)) of
      Nothing -> getter obj
      Just Refl -> value

  optField :: forall field key subTree tree obj repr proxy.
              ( KnownSymbol key
              , tree ~ '[ 'Node key 'Nullable field subTree]
              )
           => proxy key
           -> (obj -> Maybe field)
           -> repr subTree field
           -> Field (Setter queryKey queryType) obj tree (Maybe field)
  optField _ getter _ = Set $ \_ value obj ->
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, Maybe field)) of
      Nothing -> getter obj
      Just Refl -> value

  listField :: forall field key subTree tree obj repr proxy.
               ( KnownSymbol key
               , tree ~ '[ 'Node key 'List field subTree]
               )
            => proxy key
            -> (obj -> [field])
            -> repr subTree field
            -> Field (Setter queryKey queryType) obj tree [field]
  listField _ getter _ = Set $ \_ value obj ->
    case sameField (Proxy @'(queryKey, queryType)) (Proxy @'(key, [field])) of
      Nothing -> getter obj
      Just Refl -> value

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

