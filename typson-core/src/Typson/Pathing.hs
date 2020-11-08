{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Typson.Pathing
-- Description : Provides JSON pathing primitives
-- Copyright   : (c) Aaron Allen, 2020
-- Maintainer  : Aaron Allen <aaronallen8455@gmail.com>
-- License     : BSD-style (see the file LICENSE)
-- Stability   : experimental
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Typson.Pathing
  ( -- * Pathing
    -- | Components for constructing JSON paths for queries.
    type (:->)
  , type Idx
  , PathComponent(..)
  , TypeAtPath
  , typeAtPath
    -- * Path Reflection
  , ReflectPath(..)
  , sqlPath
  ) where

import           Data.Kind (Type)
import           Data.List (intercalate)
import qualified Data.List.NonEmpty as NE
import           Data.Proxy (Proxy(..))
import           GHC.TypeLits (ErrorMessage(..), KnownNat, KnownSymbol, Nat, Symbol, TypeError, natVal, symbolVal)

import           Typson.JsonTree (Aggregator(..), Edge(..), EdgeLabel(..), Multiplicity(..), Tree(..))

--------------------------------------------------------------------------------
-- Type-level PostgreSQL JSON path components
--------------------------------------------------------------------------------

-- | A type operator for building a JSON query path from multiple components.
--
-- @
--    type MyQuery = "foo" :-> "bar" `Idx` 2 :-> "baz"
-- @
data key :-> path -- key is polykinded, can be a Symbol or an Idx
infixr 4 :->

-- | A path component used to query a list field at a specific index.
data Idx (key :: Symbol) (idx :: Nat)

--------------------------------------------------------------------------------
-- Get the result type for a query at a given path
--------------------------------------------------------------------------------

typeAtPath :: proxy path
           -> repr tree obj
           -> Proxy (TypeAtPath obj tree path)
typeAtPath _ _ = Proxy

type family TypeAtPath (obj :: Type) (tree :: Tree) (path :: k) :: Type where
  -- Final key matches, return the field's type
  TypeAtPath obj
             ('Node aggr ('Edge ('Key fieldName) q field subTree ': rest))
             fieldName
    = ApQuantity q field

  -- Final array index key matches, return the field's type
  TypeAtPath obj
             ('Node 'List '[ 'Edge 'Ind 'Singleton field subTree])
             (idx :: Nat)
    = ApQuantity 'Nullable field

  -- Require an index when accessing list element
  TypeAtPath obj
             ('Node 'List '[ 'Edge 'Ind 'Singleton field subTree])
             ((idx :: Nat) :-> nextKey)
    = ApQuantity 'Nullable (TypeAtPath field subTree nextKey)

  -- List index not provided
  TypeAtPath obj
             ('Node 'List '[ 'Edge 'Ind 'Singleton field subTree])
             (key :-> nextKey)
    = TypeError ('Text "Invalid JSON path: expected a numeric index for list field, instead got string \""
           ':<>: 'Text key
           ':<>: 'Text "\"."
                )

  -- Key matches, descend into sub-object preserving Maybe
  TypeAtPath obj
             ('Node aggr ('Edge ('Key fieldName) q field subFields ': rest))
             (fieldName :-> nextKey)
    = ApQuantity q (TypeAtPath field subFields nextKey)

  -- Key doesn't match, try the next field
  TypeAtPath obj
             ('Node aggr ('Edge ('Key fieldName) q field subFields ': rest))
             key
    = TypeAtPath obj ('Node aggr rest) key

  -- No match for the key
  TypeAtPath obj tree (key :: Symbol) = TypeError (MissingKey obj key)
  TypeAtPath obj tree ((key :: Symbol) :-> path) = TypeError (MissingKey obj key)
  -- TODO improve these errors
  TypeAtPath obj tree (idx :: Nat) = TypeError ('Text "Invalid JSON path: expected a key for field on object but got a number.")
  TypeAtPath obj tree ((idx :: Nat) :-> nextKey) = TypeError ('Text "Invalid JSON path: expected a key for field on object but got a number.")

  -- Path is constructed with invalid types
  TypeAtPath obj t p = TypeError ('Text "You must use valid path syntax.")

type MissingKey obj key
  =    ('Text "JSON key not present in "
  ':<>: 'ShowType obj
  ':<>: 'Text ": \""
  ':<>: 'Text key
  ':<>: 'Text "\"" -- this is requiring UndecidableInstances
       )

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Applies a field's multiplicity to the type it contains.
-- Using this requires UndecidableInstances
type family ApQuantity (q :: Multiplicity) (b :: Type) :: Type where
  ApQuantity 'Nullable (Maybe a) = Maybe a
  ApQuantity 'Nullable a = Maybe a
  ApQuantity 'Singleton a = a

--------------------------------------------------------------------------------
-- Path Reflection
--------------------------------------------------------------------------------

data PathComponent
  = Str String
  | Idx Integer

class ReflectPath path where
  -- | Reflect a type-level path to it's value level 'PathComponent's.
  reflectPath :: proxy path -> NE.NonEmpty PathComponent

instance KnownSymbol key => ReflectPath (key :: Symbol) where
  reflectPath _ = Str (symbolVal (Proxy @key)) NE.:| []

instance KnownNat idx => ReflectPath (idx :: Nat) where
  reflectPath _ = Idx (natVal (Proxy @idx)) NE.:| []

instance (KnownSymbol key, ReflectPath path)
      => ReflectPath ((key :: Symbol) :-> path) where
  reflectPath _ = Str (symbolVal (Proxy @key))
            NE.<| reflectPath (Proxy @path)

instance (KnownNat idx, ReflectPath path)
      => ReflectPath ((idx :: Nat) :-> path) where
  reflectPath _ = Idx (natVal (Proxy @idx))
            NE.<| reflectPath (Proxy @path)

-- | Reflect a path as an SQL JSON path string
sqlPath :: ReflectPath path => proxy path -> String
sqlPath = intercalate " -> " . map pathToString . NE.toList . reflectPath
  where
    pathToString (Str s) = "'" <> s <> "'"
    pathToString (Idx i) = show i
