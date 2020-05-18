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

-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module JsonTree
  ( SomeJsonTree
  , JsonTree(..)
  , CollapseMaybes
  , TypeAtPath
  , ReflectPath(..)
  , type (.=)
  , type (.==)
  , (:->)
  , type (:->>)
  , Nat(..)
  , Nullability(..)
  , toValue
  , fromValue
  , obj
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
data (fieldName :: Symbol) .= (a :: (Nullability, Type, Nat, Type, [Type])) -- nullable, the type, depth, constructor, fields

type fieldName .== a = fieldName .= '( 'NonNull, a, 'Z, a, '[])

data Nat = Z | S Nat

data SomeJsonTree (o :: Type) (fields :: [Type]) where
  SomeJsonTree :: FromObject (Parser con, JsonTree o con con fields ('S depth)) o
               => JsonTree o con con fields ('S depth)
               -> SomeJsonTree o fields

-- Smart constructors (useless)
obj :: FromObject (Parser p, JsonTree o p p fields ('S d)) o
    => con
    -> String
    -> (JsonTree o o con '[] 'Z -> JsonTree o p p fields ('S d))
    -> SomeJsonTree o fields
obj con name fields = SomeJsonTree . fields $ EmptyObj con name

-- | Used to construct both a JSON mapping and a type level schema
-- of that object's fields
data JsonTree (o :: Type) (p :: Type) (con :: Type) (fields :: [Type]) (depth :: Nat) where
  -- | base constructor
  EmptyObj :: con -> String -> JsonTree o o con '[] 'Z

  -- | A field for a subject, adding depth to the tree
  SubObj   :: forall fieldName fields subFields f o con d sd p sp. KnownSymbol fieldName
           => (o -> f)
           -> JsonTree f sp sp subFields ('S sd)
           -> JsonTree o p con fields d
           -> JsonTree o (f -> p) con ((fieldName .= '( 'NonNull, f, 'S sd, sp, subFields)) ': fields) ('S d)

  -- | A field for a sub-object that is wrapped in some Functor
  Optional  :: forall fieldName fields subFields f o a con ap d sd p sp. KnownSymbol fieldName
            => (o -> Maybe a)
            -> JsonTree a sp sp subFields ('S sd)
            -> JsonTree o p con fields d
            -> JsonTree o (Maybe a -> p) con ((fieldName .= '( 'Nullable, a, 'S sd, sp, subFields)) ': fields) ('S d)

  -- | The leaves of the tree
  Prim     :: forall fieldName fields o f con d p. (ToJSON f, FromJSON f, KnownSymbol fieldName)
           => (o -> f)
           -> JsonTree o p con fields d
           -> JsonTree o (f -> p) con ((fieldName .== f) ': fields) ('S d)

  -- | Optional leaves
  OptPrim :: forall fieldName fields o f con d p. (ToJSON f, FromJSON f, KnownSymbol fieldName)
          => (o -> Maybe f)
          -> JsonTree o p con fields d
          -> JsonTree o (Maybe f -> p) con ((fieldName .= '( 'Nullable, f, 'Z, f, '[])) ': fields) ('S d)

getNameAndConstructor :: JsonTree o p con fields d -> (String, con)
getNameAndConstructor (EmptyObj c n) = (n, c)
getNameAndConstructor (SubObj _ _ r) = getNameAndConstructor r
getNameAndConstructor (Optional _ _ r) = getNameAndConstructor r
getNameAndConstructor (Prim _ r) = getNameAndConstructor r
getNameAndConstructor (OptPrim _ r) = getNameAndConstructor r

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

fromValue' :: SomeJsonTree o fields -> Value -> Parser o
fromValue' (SomeJsonTree t) = fromValue t

fromValue :: FromObject (Parser con, JsonTree o con con fields ('S d)) o
           => JsonTree o con con fields ('S d) -> Value -> Parser o
fromValue tree =
  let (name, con) = getNameAndConstructor tree
   in withObject name (fromObject (pure @Parser con, tree))

class FromObject t o where
  fromObject :: t -> Object -> Parser o

instance FromObject (Parser o, JsonTree o o con fields 'Z) o where
  fromObject (p, EmptyObj _ _) _ = p

instance ( FromObject (Parser p, JsonTree o p con rest d) o
         , FromObject (Parser sp, JsonTree a sp sp sub sd) a
         )
  => FromObject ( Parser (a -> p)
                , JsonTree o (a -> p)
                             con
                             ((fName .= '( 'NonNull, a, sd, sp, sub)) ': rest)
                             ('S d)
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

instance ( FromObject (Parser p, JsonTree o p con rest d) o
         , FromObject (Parser sp, JsonTree a sp sp sub sd) a
         )
  => FromObject ( Parser (Maybe a -> p)
                , JsonTree o (Maybe a -> p)
                             con
                             ((fName .= '( 'Nullable, a, sd, sp, sub)) ': rest)
                             ('S d)
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

instance KnownSymbol fieldName => GetFieldName (JsonTree o p con (fieldName .= v ': rest) d) where
  getFieldName _ = T.pack (symbolVal (Proxy :: Proxy fieldName))

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

toValue' :: SomeJsonTree a fields -> a -> Value
toValue' (SomeJsonTree t) = toValue t

toObject :: JsonTree a p con fields d -> a -> Object
toObject EmptyObj{} _ = mempty
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

toValue :: JsonTree a p con fields d -> a -> Value
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
                       ((fieldName .= '(nul, field, sd, sc, subFields)) ': rest)
                       d
             )
             (fieldName :-> ())
    = ApNull nul field

  -- Key matches, descend into sub-object preserving Maybe
  TypeAtPath (JsonTree o p con
                       ((fieldName .= '(nul, field, sd, sc, subFields)) ': rest)
                       d
             )
             (fieldName :-> nextKey)
    = ApNull nul (TypeAtPath (JsonTree field sc sc subFields sd) nextKey)

  -- Key doesn't match, try the next field
  TypeAtPath (JsonTree o (x -> p) con
                         ((fieldName .= '(nul, field, subFields, sd, sc)) ': rest)
                         ('S d)
             )
             (key :-> nextKey)
    = TypeAtPath (JsonTree o p con rest d) (key :-> nextKey)

  -- No match for the key
  TypeAtPath (JsonTree o p con '[] d) (key :-> path)
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

