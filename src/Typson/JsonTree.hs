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
  , type Quantity(..)
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

import           Data.Aeson ((.:), (.:?), (.=), FromJSON, ToJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

--------------------------------------------------------------------------------
-- Type-level JSON Tree Representation
--------------------------------------------------------------------------------

type Tree = [Node]

data Node
  = Node Symbol Quantity Type Tree

data Quantity
  = List
  | Singleton
  | Nullable

--------------------------------------------------------------------------------
-- Final-tagless Symantics for Object Construction
--------------------------------------------------------------------------------

class ObjectSYM (repr :: Tree -> Type -> Type) where
  object :: NoDuplicateKeys o t
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
  prim key acc = FieldEncoder $ \o -> T.pack (symbolVal key) .= acc o
  optPrim key acc = FieldEncoder $ \o -> T.pack (symbolVal key) .= acc o
  subObj key acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal key) .= so (acc o)
  optSubObj key acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal key) .= (so <$> acc o)
  subObjList key acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal key) .= (so <$> acc o)

instance FieldSYM ObjectDecoder where
  newtype Field ObjectDecoder o t a =
    FieldDecoder { unFieldDecoder :: Aeson.Object -> Aeson.Parser a }
  prim key _ = FieldDecoder $ \obj ->
    obj .: T.pack (symbolVal key)
  optPrim key _ = FieldDecoder $ \obj ->
    obj .:? T.pack (symbolVal key)
  subObj key _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal key)
    d so
  optSubObj key _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    mbSo <- obj .:? T.pack (symbolVal key)
    traverse d mbSo
  subObjList key _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal key)
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
    = TypeError (Text "Duplicate JSON key \""
            :<>: Text key
            :<>: Text "\" in object "
            :<>: ShowType obj
                )
  KeyNotPresent key obj ('Node notKey q ty subTree ': rest)
    = KeyNotPresent key obj rest
  KeyNotPresent key obj '[] = ()

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

runAp_ :: Monoid m => (forall a t. f t a -> m) -> IFreeAp f t a -> m
runAp_ _ (Pure _) = mempty
runAp_ f (Ap p c) = runAp_ f p <> f c

runAp :: Applicative g => (forall a t. f t a -> g a) -> IFreeAp f t a -> g a
runAp _ (Pure a) = pure a
runAp f (Ap p c) = runAp f p <*> f c

