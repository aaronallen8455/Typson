{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pathing
  ( ReflectPath(..)
  , CollapseMaybes
  , TypeAtPath
  , typeAtPath
  , (:->)
  , (:->>)
  ) where

import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

import           JsonTreeIndexed (Node(..), Nullability(..), ObjectSYM, Tree)

typeAtPath :: forall path obj tree repr.
              ObjectSYM repr
           => repr tree obj
           -> Proxy (CollapseMaybes (TypeAtPath obj tree path))
typeAtPath _ = Proxy

data (key :: Symbol) :-> path
infixr 4 :->
type key :->> lastKey = key :-> lastKey :-> ()

type family TypeAtPath (obj :: Type) (tree :: Tree) path :: Type where
  -- Final key matches, return the field's type
  TypeAtPath obj
             ('Node fieldName nul field subTree ': rest)
             (fieldName :-> ())
    = ApNull nul field

  -- Key matches, descend into sub-object preserving Maybe
  TypeAtPath obj
             ('Node fieldName nul field subFields ': rest)
             (fieldName :-> nextKey)
    = ApNull nul (TypeAtPath field subFields nextKey)

  -- Key doesn't match, try the next field
  TypeAtPath obj
             ('Node fieldName nul field subFields ': rest)
             (key :-> nextKey)
    = TypeAtPath obj rest (key :-> nextKey)

  -- No match for the key
  TypeAtPath obj '[] (key :-> path)
    = TypeError (Text "JSON key not present in "
            :<>: ShowType (RemoveMaybes obj)
            :<>: Text ": \""
            :<>: Text key
            :<>: Text "\"" -- this is requiring UndecidableInstances
                )

-- Using this requires UndecidableInstances
type family ApNull (a :: Nullability) (b :: Type) :: Type where
  ApNull Nullable a = Maybe a
  ApNull NonNullable a = a

-- This limits to one level of maybeness along a path.
type family CollapseMaybes a :: Type where
  CollapseMaybes (Maybe (Maybe a)) = CollapseMaybes (Maybe a)
  CollapseMaybes (Maybe a) = Maybe a
  CollapseMaybes a = a

type family RemoveMaybes a :: Type where
  RemoveMaybes (Maybe a) = RemoveMaybes a
  RemoveMaybes a = a

class ReflectPath f where
  reflectPath :: Proxy f -> [T.Text]

instance ReflectPath () where
  reflectPath _ = []

instance (KnownSymbol key, ReflectPath path) => ReflectPath (key :-> path) where
  reflectPath _ = T.pack (symbolVal (Proxy :: Proxy key))
                : reflectPath (Proxy :: Proxy path)
