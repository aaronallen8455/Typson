{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
module JsonTree
  ( SomeJsonTree
  , JsonTree(..)
  , CollapseMaybes
  , TypeAtPath
  , ReflectPath(..)
  , type (.=)
  , type (.==)
  , type (:->)
  , type (:->>)
  , Nat(..)
  , Nullability(..)
  , toValue
  , fromValue
  , obj
  , JTree
  ) where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Traversable (for)
import           GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

--------------------------------------------------------------------------------
-- JsonTree Types
--------------------------------------------------------------------------------

data Nullability = NonNull | Nullable

type family ApNull (a :: Nullability) (b :: Type) :: Type where
  ApNull Nullable a = Maybe a
  ApNull NonNull a = a

-- | Type level key value pairs
data (fieldName :: Symbol) .= (a :: (Nullability, Type, Type, [Type])) -- nullable, the type, constructor, fields

type fieldName .== a = fieldName .= '( 'NonNull, a, a, '[])

data Nat = Z | S Nat

type JTree o f con = JsonTree o con con f

data SomeJsonTree (o :: Type) (fields :: [Type]) where
  SomeJsonTree :: FromObject (Parser con, JsonTree o con con fields) o
               => JsonTree o con con fields
               -> SomeJsonTree o fields

-- Smart constructors (useless)
obj :: FromObject (Parser p, JsonTree o p p fields) o
    => con
    -> String
    -> (JsonTree o o con '[] -> JsonTree o p p fields)
    -> SomeJsonTree o fields
obj con name fields = SomeJsonTree . fields $ Obj con name

-- | Used to construct both a JSON mapping and a type level schema
-- of that object's fields
data JsonTree (o :: Type) (p :: Type) (con :: Type) (fields :: [Type]) where
  -- | base constructor
  Obj :: con -> String -> JsonTree o o con '[]

  -- | A field for a subject, adding depth to the tree
  SubObj   :: forall fieldName fields subFields f o con p sp. KnownSymbol fieldName
           => (o -> f)
           -> JsonTree f sp sp subFields
           -> JsonTree o p con fields
           -> JsonTree o (f -> p) con ((fieldName .= '( 'NonNull, f, sp, subFields)) ': fields)

  -- | A field for a sub-object that is wrapped in some Functor
  Optional  :: forall fieldName fields subFields f o a con ap p sp. KnownSymbol fieldName
            => (o -> Maybe a)
            -> JsonTree a sp sp subFields
            -> JsonTree o p con fields
            -> JsonTree o (Maybe a -> p) con ((fieldName .= '( 'Nullable, a, sp, subFields)) ': fields)

  -- | The leaves of the tree
  Prim     :: forall fieldName fields o f con p. (ToJSON f, FromJSON f, KnownSymbol fieldName)
           => (o -> f)
           -> JsonTree o p con fields
           -> JsonTree o (f -> p) con ((fieldName .== f) ': fields)

  -- | Optional leaves
  OptPrim :: forall fieldName fields o f con p. (ToJSON f, FromJSON f, KnownSymbol fieldName)
          => (o -> Maybe f)
          -> JsonTree o p con fields
          -> JsonTree o (Maybe f -> p) con ((fieldName .= '( 'Nullable, f, f, '[])) ': fields)

getNameAndConstructor :: JsonTree o p con fields -> (String, con)
getNameAndConstructor (Obj c n) = (n, c)
getNameAndConstructor (SubObj _ _ r) = getNameAndConstructor r
getNameAndConstructor (Optional _ _ r) = getNameAndConstructor r
getNameAndConstructor (Prim _ r) = getNameAndConstructor r
getNameAndConstructor (OptPrim _ r) = getNameAndConstructor r

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

fromValue' :: SomeJsonTree o fields -> Value -> Parser o
fromValue' (SomeJsonTree t) = fromValue t

fromValue :: FromObject (Parser con, JsonTree o con con fields) o
           => JsonTree o con con fields -> Value -> Parser o
fromValue tree =
  let (name, con) = getNameAndConstructor tree
   in withObject name (fromObject (pure @Parser con, tree))

class FromObject t o where
  fromObject :: t -> Object -> Parser o

instance FromObject (Parser o, JsonTree o o con fields) o where
  fromObject (p, Obj _ _) _ = p

instance ( FromObject (Parser p, JsonTree o p con rest) o
         , FromObject (Parser sp, JsonTree a sp sp sub) a
         )
  => FromObject ( Parser (a -> p)
                , JsonTree o (a -> p)
                             con
                             ((fName .= '( 'NonNull, a, sp, sub)) ': rest)
                ) o where
  fromObject (p, SubObj _ subTree nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = do
          v <- obj .: T.pack (symbolVal (Proxy :: Proxy fName))
          fromValue subTree v
  fromObject (p, Prim _ nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = obj .: T.pack (symbolVal (Proxy :: Proxy fName))

instance ( FromObject (Parser p, JsonTree o p con rest) o
         , FromObject (Parser sp, JsonTree a sp sp sub) a
         )
  => FromObject ( Parser (Maybe a -> p)
                , JsonTree o (Maybe a -> p)
                             con
                             ((fName .= '( 'Nullable, a, sp, sub)) ': rest)
                ) o where
  fromObject (p, Optional _ subTree nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = do
          mbV <- obj .:? T.pack (symbolVal (Proxy :: Proxy fName))
          for mbV $ \v ->
            fromValue subTree v
  fromObject (p, OptPrim _ nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = obj .:? T.pack (symbolVal (Proxy :: Proxy fName))

class GetFieldName f where
  getFieldName :: f -> T.Text

instance KnownSymbol fieldName => GetFieldName (JsonTree o p con (fieldName .= v ': rest)) where
  getFieldName _ = T.pack (symbolVal (Proxy :: Proxy fieldName))

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

toValue' :: SomeJsonTree a fields -> a -> Value
toValue' (SomeJsonTree t) = toValue t

toObject :: JsonTree a p con fields -> a -> Object
toObject Obj{} _ = mempty
toObject t@(SubObj acc subTree rest) obj
  = getFieldName t .= Object (toObject subTree $ acc obj)
 <> toObject rest obj
toObject t@(Prim acc rest) obj
  = getFieldName t .= (toJSON $ acc obj)
 <> toObject rest obj
toObject t@(OptPrim acc rest) obj
  = getFieldName t .= (toJSON $ acc obj)
 <> toObject rest obj
toObject t@(Optional acc subTree rest) obj
  = getFieldName t .= toJSON (Object . toObject subTree <$> acc obj)
 <> toObject rest obj

toValue :: JsonTree a p con fields -> a -> Value
toValue t = Object . toObject t

--------------------------------------------------------------------------------
-- Pathing
--------------------------------------------------------------------------------

data (key :: Symbol) :-> path
infixr 4 :->
type key :->> lastKey = key :-> lastKey :-> ()

type family TypeAtPath obj path :: Type where
  -- Final key matches, return the field's type
  TypeAtPath (JsonTree o p con
                       ((fieldName .= '(nul, field, sc, subFields)) ': rest)
             )
             (fieldName :-> ())
    = ApNull nul field

  -- Key matches, descend into sub-object preserving Maybe
  TypeAtPath (JsonTree o p con
                       ((fieldName .= '(nul, field, sc, subFields)) ': rest)
             )
             (fieldName :-> nextKey)
    = ApNull nul (TypeAtPath (JsonTree field sc sc subFields) nextKey)

  -- Key doesn't match, try the next field
  TypeAtPath (JsonTree o (x -> p) con
                         ((fieldName .= '(nul, field, subFields, sc)) ': rest)
             )
             (key :-> nextKey)
    = TypeAtPath (JsonTree o p con rest) (key :-> nextKey)

  -- No match for the key
  TypeAtPath (JsonTree o p con '[]) (key :-> path)
    = TypeError (Text "JSON key not present in "
            :<>: ShowType (RemoveMaybes o)
            :<>: Text ": \""
            :<>: Text key
            :<>: Text "\"" -- this is requiring UndecidableInstances
                )

-- This limits to one level of maybeness along a path.
type family CollapseMaybes a :: Type where
  CollapseMaybes (Maybe (Maybe a)) = CollapseMaybes (Maybe a)
  CollapseMaybes (Maybe a) = Maybe a
  CollapseMaybes a = a

type family RemoveMaybes a :: Type where
  RemoveMaybes (Maybe a) = RemoveMaybes a
  RemoveMaybes a = a

typeAtPath :: forall path obj. obj -> Proxy (CollapseMaybes (TypeAtPath obj path))
typeAtPath _ = Proxy

--------------------------------------------------------------------------------
-- Get value at path
--------------------------------------------------------------------------------

class ReflectPath f where
  reflectPath :: Proxy f -> [T.Text]

instance ReflectPath () where
  reflectPath _ = []

instance (KnownSymbol key, ReflectPath path) => ReflectPath (key :-> path) where
  reflectPath _ = T.pack (symbolVal (Proxy :: Proxy key))
                : reflectPath (Proxy :: Proxy path)

atPath :: forall path obj. (ReflectPath path, FromJSON (CollapseMaybes (TypeAtPath obj path)))
       => Value -> obj -> Maybe (CollapseMaybes (TypeAtPath obj path))
atPath (Object obj) _ =
  let path = reflectPath (Proxy :: Proxy path)
      go [] _ = Nothing
      go [p] obj = do
        v <- HM.lookup p obj
        Success a <- Just $ fromJSON v
        pure a
      go (p:ps) obj = do
        Object o <- HM.lookup p obj
        go ps o
   in go path obj
atPath _ _ = Nothing

