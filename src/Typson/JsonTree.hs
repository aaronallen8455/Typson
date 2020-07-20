{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for the custom type error
module Typson.JsonTree
  ( Tree
  , JsonTree
  , type Node(..)
  , type Multiplicity(..)
  , ObjectSYM(..)
  , FieldSYM(..)
  , encodeObject
  , decodeObject
  , getObjectTree
  , key
  , (<<$>)
  , (<<*>)
  , runAp
  , runAp_
  ) where

import           Data.Aeson ((.!=), (.:), (.:?), (.=), FromJSON, ToJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Type.Bool (If)
import           GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

--------------------------------------------------------------------------------
-- Type-level JSON Tree Representation
--------------------------------------------------------------------------------

type Tree = [Node]

data Node
  = Node Symbol Multiplicity Type Tree

data Multiplicity
  = List
  | Singleton
  | Nullable

--------------------------------------------------------------------------------
-- Final-tagless Symantics for Object Construction
--------------------------------------------------------------------------------

class ObjectSYM (repr :: Tree -> Type -> Type) where
  object :: (NonRecursive '[o] t, NoDuplicateKeys o t)
         => String -> IFreeAp (Field repr o) t o -> repr t o

class FieldSYM repr where
  data Field repr :: Type -> Tree -> Type -> Type

  prim :: ( FromJSON field
          , ToJSON field
          , KnownSymbol key
          , tree ~ '[ 'Node key 'Singleton field '[]]
          )
       => proxy key
       -> (obj -> field)
       -> Field repr obj tree field

  optPrim :: ( FromJSON field
             , ToJSON field
             , KnownSymbol key
             , tree ~ '[ 'Node key 'Nullable field '[]]
             )
          => proxy key
          -> (obj -> Maybe field)
          -> Field repr obj tree (Maybe field)

  optPrimDef :: ( FromJSON field
                , ToJSON field
                , KnownSymbol key
                , tree ~ '[ 'Node key 'Singleton field '[]]
                )
             => proxy key
             -> (obj -> field)
             -> field
             -> Field repr obj tree field
  optPrimDef p getter _ = prim p getter

  subObj :: ( KnownSymbol key
            , tree ~ '[ 'Node key 'Singleton field subTree]
            )
         => proxy key
         -> (obj -> field)
         -> repr subTree field
         -> Field repr obj tree field

  optSubObj :: ( KnownSymbol key
               , tree ~ '[ 'Node key 'Nullable field subTree]
               )
            => proxy key
            -> (obj -> Maybe field)
            -> repr subTree field
            -> Field repr obj tree (Maybe field)

  optSubObjDef :: ( KnownSymbol key
                  , tree ~ '[ 'Node key 'Singleton field subTree]
                  )
               => proxy key
               -> (obj -> field)
               -> field
               -> repr subTree field
               -> Field repr obj tree field
  optSubObjDef p getter _ sub = subObj p getter sub

  subObjList :: ( KnownSymbol key
                , tree ~ '[ 'Node key 'List field subTree]
                )
             => proxy key
             -> (obj -> [field])
             -> repr subTree field
             -> Field repr obj tree [field]

type JsonTree t a = forall repr. (ObjectSYM repr, FieldSYM repr) => repr t a

key :: Proxy (key :: Symbol)
key = Proxy

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

newtype ObjectEncoder (t :: Tree) o =
  ObjectEncoder { encodeObject :: o -> Aeson.Value }

newtype ObjectDecoder (t :: Tree) o =
  ObjectDecoder { decodeObject :: Aeson.Value -> Aeson.Parser o }

data TreeProxy (t :: Tree) o = TreeProxy

newtype ObjectTree (t :: Tree) o =
  ObjectTree { getObjectTree :: TreeProxy t o }

instance ObjectSYM ObjectEncoder where
  object _ fields = ObjectEncoder $ \o ->
    Aeson.Object $ runAp_ (`unFieldEncoder` o) fields

instance ObjectSYM ObjectDecoder where
  object name fields = ObjectDecoder . Aeson.withObject name $ \obj ->
    runAp (`unFieldDecoder` obj) fields

instance ObjectSYM ObjectTree where
  object _ _ = ObjectTree TreeProxy

instance FieldSYM ObjectTree where
  data Field ObjectTree o t a = FieldProxy
  prim _ _ = FieldProxy
  optPrim _ _ = FieldProxy
  subObj _ _ _ = FieldProxy
  optSubObj _ _ _ = FieldProxy
  subObjList _ _ _ = FieldProxy

instance FieldSYM ObjectEncoder where
  newtype Field ObjectEncoder o t a =
    FieldEncoder { unFieldEncoder :: o -> Aeson.Object }
  prim ky acc = FieldEncoder $ \o -> T.pack (symbolVal ky) .= acc o
  optPrim ky acc = FieldEncoder $ \o -> T.pack (symbolVal ky) .= acc o
  subObj ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= so (acc o)
  optSubObj ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= (so <$> acc o)
  subObjList ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= (so <$> acc o)

instance FieldSYM ObjectDecoder where
  newtype Field ObjectDecoder o t a =
    FieldDecoder { unFieldDecoder :: Aeson.Object -> Aeson.Parser a }
  prim ky _ = FieldDecoder $ \obj ->
    obj .: T.pack (symbolVal ky)
  optPrim ky _ = FieldDecoder $ \obj ->
    obj .:? T.pack (symbolVal ky)
  optPrimDef ky _ def = FieldDecoder $ \obj ->
    obj .:? T.pack (symbolVal ky) .!= def
  subObj ky _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal ky)
    d so
  optSubObj ky _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    mbSo <- obj .:? T.pack (symbolVal ky)
    traverse d mbSo
  optSubObjDef ky _ def (ObjectDecoder d) = FieldDecoder $ \obj -> do
    mbSo <- obj .:? T.pack (symbolVal ky)
    maybe (pure def) d mbSo
  subObjList ky _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal ky)
    traverse d so

--------------------------------------------------------------------------------
-- No Duplicate Keys Constraint
--------------------------------------------------------------------------------

type family NoDuplicateKeys (obj :: Type) (tree :: Tree) :: Constraint where
  NoDuplicateKeys obj ('Node key q ty subTree ': rest)
    = (KeyNotPresent key obj rest, NoDuplicateKeys obj rest)
  NoDuplicateKeys obj '[] = ()

type family KeyNotPresent (key :: Symbol) (obj :: Type) (tree :: Tree) :: Constraint where
  KeyNotPresent key obj ('Node key q ty subTree ': rest)
    = TypeError ('Text "Duplicate JSON key \""
            ':<>: 'Text key
            ':<>: 'Text "\" in object "
            ':<>: 'ShowType obj
                )
  KeyNotPresent key obj ('Node notKey q ty subTree ': rest)
    = KeyNotPresent key obj rest
  KeyNotPresent key obj '[] = ()

--------------------------------------------------------------------------------
-- No Recursion Constraint
--------------------------------------------------------------------------------

-- TODO Is a type level Set available?
type family NonRecursive (visited :: [Type]) (tree :: Tree) :: Constraint where
  NonRecursive visited ('Node key q ty subTree ': rest)
    = If (Elem ty visited)
         (TypeError ('Text "Recursive JSON types are not allowed."))
         (NonRecursive visited rest, NonRecursive (ty ': visited) subTree)
  NonRecursive visited '[] = ()

type family Elem (needle :: Type) (haystack :: [Type]) :: Bool where
  Elem needle (needle ': rest) = 'True
  Elem needle (head ': rest) = Elem needle rest
  Elem needle '[] = 'False

-- TODO can the two constraints be melded?

--------------------------------------------------------------------------------
-- Free Indexed Applicative
--------------------------------------------------------------------------------

data IFreeAp f (t :: Tree) (a :: Type) where
  Pure :: a -> IFreeAp f '[] a
  Ap   :: IFreeAp f t (a -> b)
       -> f '[st] a
       -> IFreeAp f (st ': t) b

(<<$>) :: (a -> b) -> f '[st] a -> IFreeAp f '[st] b
f <<$> i = Pure f `Ap` i
infixl 4 <<$>

(<<*>) :: IFreeAp f t (a -> b) -> f '[st] a -> IFreeAp f (st : t) b
(<<*>) = Ap
infixl 4 <<*>

runAp_ :: Monoid m => (forall a' t'. f t' a' -> m) -> IFreeAp f t a -> m
runAp_ _ (Pure _) = mempty
runAp_ f (Ap p c) = runAp_ f p <> f c

runAp :: Applicative g => (forall a' t'. f t' a' -> g a') -> IFreeAp f t a -> g a
runAp _ (Pure a) = pure a
runAp f (Ap p c) = runAp f p <*> f c

