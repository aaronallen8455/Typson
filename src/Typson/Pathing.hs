{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Typson.Pathing
  ( ReflectPath(..)
  , TypeAtPath
  , typeAtPath
  , sqlPath
  , PathComponent(..)
  , type (:->)
  , type (:->>)
  , type Idx
  ) where

import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)

import           Typson.JsonTree (Node(..), Multiplicity(..), Tree)

--------------------------------------------------------------------------------
-- Type-level PostgreSQL JSON path components
--------------------------------------------------------------------------------

data key :-> path -- key is polykinded, can be a Symbol or an Idx
infixr 4 :->
type key :->> lastKey = key :-> lastKey :-> ()
infixr 4 :->>

data Idx (key :: Symbol) (idx :: Nat) -- used to access element of a list

--------------------------------------------------------------------------------
-- Get the result type for a query at a given path
--------------------------------------------------------------------------------

typeAtPath :: forall path obj tree repr.
              repr tree obj
           -> Proxy (TypeAtPath obj tree path)
typeAtPath _ = Proxy

type family TypeAtPath (obj :: Type) (tree :: Tree) path :: Type where
  -- Final key matches, return the field's type
  TypeAtPath obj
             ('Node fieldName q field subTree ': rest)
             (fieldName :-> ())
    = ApResult q field

  -- Final array index key matches, return the field's type
  TypeAtPath obj
             ('Node fieldName 'List field subTree ': rest)
             (Idx fieldName idx :-> ())
    = ApQuantity 'List field

  -- Final array index key for primitive list field matches, return the field's type
  TypeAtPath obj
             ('Node fieldName q [field] '[] ': rest)
             (Idx fieldName idx :-> ())
    = ApQuantity 'List field

  -- Require an index when accessing list element
  TypeAtPath obj
             ('Node fieldName 'List field subTree ': rest)
             (Idx fieldName idx :-> nextKey)
    = ApQuantity 'List (TypeAtPath field subTree nextKey)

  -- List index not provided
  TypeAtPath obj
             ('Node key 'List field subTree ': rest)
             (key :-> nextKey)
    = TypeError ('Text "Invalid JSON path: you must provide an index for list field \""
           ':<>: 'Text key
           ':<>: 'Text "\" of object "
           ':<>: 'ShowType obj
                )

  -- Key matches, descend into sub-object preserving Maybe
  TypeAtPath obj
             ('Node fieldName q field subFields ': rest)
             (fieldName :-> nextKey)
    = ApQuantity q (TypeAtPath field subFields nextKey)

  -- Key doesn't match, try the next field
  TypeAtPath obj
             ('Node fieldName q field subFields ': rest)
             (key :-> nextKey)
    = TypeAtPath obj rest (key :-> nextKey)

  -- No match for key with list index
  TypeAtPath obj '[] (Idx key idx :-> nextKey)
    = TypeError ('Text "JSON key not present in "
           ':<>: 'ShowType obj
           ':<>: 'Text ": \""
           ':<>: 'Text key
           ':<>: 'Text "\""
                )

  -- No match for the key
  TypeAtPath obj '[] (key :-> path)
    = TypeError ('Text "JSON key not present in "
           ':<>: 'ShowType obj
           ':<>: 'Text ": \""
           ':<>: 'Text key
           ':<>: 'Text "\"" -- this is requiring UndecidableInstances
                )

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

type family ApResult (q :: Multiplicity) (b :: Type) :: Type where
  ApResult 'List a = [a]
  ApResult q a = ApQuantity q a

-- Using this requires UndecidableInstances
type family ApQuantity (q :: Multiplicity) (b :: Type) :: Type where
  ApQuantity 'Nullable (Maybe a) = Maybe a
  ApQuantity 'Nullable a = Maybe a
  ApQuantity 'Singleton a = a
  ApQuantity 'List (Maybe a) = Maybe a
  ApQuantity 'List a = Maybe a

--------------------------------------------------------------------------------
-- Path Reflection
--------------------------------------------------------------------------------

data PathComponent
  = Key String
  | Idx Integer

class ReflectPath f where
  reflectPath :: proxy f -> [PathComponent]

instance ReflectPath () where
  reflectPath _ = []

instance (KnownSymbol key, ReflectPath path)
      => ReflectPath ((key :: Symbol) :-> path) where
  reflectPath _ = Key (symbolVal (Proxy @key))
                : reflectPath (Proxy @path)

instance (KnownSymbol key, KnownNat idx, ReflectPath path)
      => ReflectPath (Idx key idx :-> path) where
  reflectPath _ = Key (symbolVal (Proxy @key))
                : Idx (natVal (Proxy @idx))
                : reflectPath (Proxy @path)

-- Reflect a path as a postgres SQL string
sqlPath :: ReflectPath path => proxy path -> String
sqlPath = buildPath . map pathToString . reflectPath
  where
    buildPath [a, b] = a <> " -> " <> b
    buildPath [a] = a
    buildPath (a : rest) = a <> " -> " <> buildPath rest
    buildPath [] = "" -- TODO could use non-empty list

    pathToString (Key s) = "'" <> s <> "'"
    pathToString (Idx i) = show i
