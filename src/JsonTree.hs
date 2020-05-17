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

-- {-# LANGUAGE PartialTypeSignatures #-}
-- {-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module JsonTree
  ( JsonTree(..)
  , CollapseMaybes
  , TypeAtPath
  , ReflectPath(..)
  , type (.=)
  , type (.==)
  , (:->)
  , type (:->>)
  , toValue
  ) where

import           Data.Aeson
import qualified Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

--------------------------------------------------------------------------------
-- JsonTree Types
--------------------------------------------------------------------------------

-- | Type level key value pairs
data (fieldName :: Symbol) .= (a :: (Type, [Type]))

type fieldName .== a = fieldName .= '(a, '[])

-- | Used to construct both a JSON mapping and a type level schema
-- of that object's fields
data JsonTree (o :: Type) (parser :: Type) (fields :: [Type]) where
  -- | base constructor
  EmptyObj :: JsonTree o p '[]
  CompleteObj :: String -> con -> JsonTree o (Uncurry con o) fields -> JsonTree  JsonTree o o fields
  -- | A field for a subject, adding depth to the tree
  SubObj   :: forall fieldName fields subFields f o c. KnownSymbol fieldName
           => (o -> f)
           -> JsonTree f f subFields
           -> JsonTree o (f -> c) fields
           -> JsonTree o c ((fieldName .= '(f, subFields)) ': fields)
  -- | A field for a sub-object that is wrapped in some Functor
  Optional  :: forall fieldName fields subFields f o a c. KnownSymbol fieldName
            => (o -> Maybe a)
            -> JsonTree a a subFields
            -> JsonTree o (a -> c) fields
            -> JsonTree o c ((fieldName .= '(Maybe a, subFields)) ': fields)
  -- | The leaves of the tree
  Prim     :: forall fieldName fields o f c. (ToJSON f, KnownSymbol fieldName)
           => (o -> f)
           -> JsonTree o (f -> c) fields
           -> JsonTree o c ((fieldName .== f) ': fields)

type JsonTree' o fields = JsonTree o o fields

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

toObject :: JsonTree a p fields -> a -> Object
toObject EmptyObj{} _ = mempty
toObject t@(SubObj acc subTree rest) obj
  = getFieldName t .= Object (toObject subTree $ acc obj)
 <> toObject rest obj
toObject t@(Prim acc rest) obj
  = getFieldName t .= (toJSON $ acc obj)
 <> toObject rest obj
toObject t@(Optional acc subTree rest) obj
  = getFieldName t .= toJSON (Object . toObject subTree <$> acc obj)
 <> toObject rest obj

toValue :: JsonTree' a fields -> a -> Value
toValue t = Object . toObject t

class GetFieldName f where
  getFieldName :: f -> T.Text

instance KnownSymbol fieldName => GetFieldName (JsonTree o p (fieldName .= v ': rest)) where
  getFieldName _ = T.pack (symbolVal (Proxy :: Proxy fieldName))

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Pathing
--------------------------------------------------------------------------------

data (key :: Symbol) :-> path
infixr 4 :->
type key :->> lastKey = key :-> lastKey :-> ()

type family TypeAtPath obj path :: Type where
  -- Final key matches, return the field's type preserving Maybe
  TypeAtPath (JsonTree' (Maybe o)
                       ((fieldName .= '(field, subFields)) ': rest)
             )
             (fieldName :-> ())
    = Maybe field

  -- Final key matches, return the field's type
  TypeAtPath (JsonTree' o
                       ((fieldName .= '(field, subFields)) ': rest)
             )
             (fieldName :-> ())
    = field

  -- Key matches, descend into sub-object preserving Maybe
  TypeAtPath (JsonTree' (Maybe o)
                       ((fieldName .= '(field, subFields)) ': rest)
             )
             (fieldName :-> nextKey)
    = TypeAtPath (JsonTree' (Maybe field) subFields) nextKey

  -- Key matches, descend into sub-object
  TypeAtPath (JsonTree' o
                       ((fieldName .= '(field, subFields)) ': rest)
             )
             (fieldName :-> nextKey)
    = TypeAtPath (JsonTree' field subFields) nextKey

  -- Key doesn't match, try the next field
  TypeAtPath (JsonTree' o
                       ((fieldName .= '(field, subFields)) ': rest)
             )
             (key :-> nextKey)
    = TypeAtPath (JsonTree' o rest) (key :-> nextKey)

  -- No match for the key
  TypeAtPath (JsonTree' o '[]) (key :-> path)
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

