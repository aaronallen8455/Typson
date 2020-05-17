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

module JsonTree where
--  ( JsonTree(..)
--  , CollapseMaybes
--  , TypeAtPath
--  , ReflectPath(..)
--  , type (.=)
--  , type (.==)
--  , (:->)
--  , type (:->>)
--  , toValue
--  ) where

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
  ApNull NonNull (Maybe a) = a

-- | Type level key value pairs
data (fieldName :: Symbol) .= (a :: (Nullability, Type, Nat, Type, [Type])) -- nullable, the type, depth, constructor, fields

type fieldName .== a = fieldName .= '( 'NonNull, a, 'Z, a, '[])

data Nat = Z | S Nat

-- | Used to construct both a JSON mapping and a type level schema
-- of that object's fields
data JsonTree (o :: Type) (p :: Type) (fields :: [Type]) (depth :: Nat) where
  -- | base constructor
  EmptyObj :: String -> JsonTree o o '[] 'Z
  -- | A field for a subject, adding depth to the tree
  SubObj   :: forall fieldName fields subFields f o c fp d sd p sp. KnownSymbol fieldName
           => (o -> f)
           -> sp
           -> JsonTree f sp subFields ('S sd)
           -> JsonTree o p fields d
           -> JsonTree o (f -> p) ((fieldName .= '( 'NonNull, f, 'S sd, sp, subFields)) ': fields) ('S d)
  -- | A field for a sub-object that is wrapped in some Functor
  Optional  :: forall fieldName fields subFields f o a c ap d sd p sp. KnownSymbol fieldName
            => (o -> Maybe a)
            -> sp
            -> JsonTree a sp subFields ('S sd)
            -> JsonTree o p fields d
            -> JsonTree o (Maybe a -> p) ((fieldName .= '( 'Nullable, a, 'S sd, sp, subFields)) ': fields) ('S d)
  -- | The leaves of the tree
  Prim     :: forall fieldName fields o f c d p. (ToJSON f, FromJSON f, KnownSymbol fieldName)
           => (o -> f)
           -> JsonTree o p fields d
           -> JsonTree o (f -> p) ((fieldName .== f) ': fields) ('S d)
-- TODO need OptionalPrim

getObjName :: JsonTree o p fields d -> String
getObjName (EmptyObj n) = n
getObjName (SubObj _ _ _ r) = getObjName r
getObjName (Optional _ _ _ r) = getObjName r
getObjName (Prim _ r) = getObjName r

fromValue :: FromObject (Parser con, JsonTree o con fields ('S d)) o
          => con -> JsonTree o con fields ('S d) -> Value -> Parser o
fromValue con tree = withObject (getObjName tree) (fromObject (pure @Parser con, tree))

class FromObject t o where
  fromObject :: t -> Object -> Parser o

instance FromObject (Parser o, JsonTree o o fields 'Z) o where
  fromObject (p, EmptyObj _) _ = p

instance ( FromObject (Parser p, JsonTree o p rest d) o
         , FromObject (Parser sp, JsonTree a sp sub sd) a
         )
  => FromObject ( Parser (a -> p)
                , JsonTree o (a -> p)
                             ((fName .= '( 'NonNull, a, sd, sp, sub)) ': rest)
                             ('S d)
                ) o where
  fromObject (p, SubObj _ subCon subTree nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = do
          v <- obj .: T.pack (symbolVal (Proxy :: Proxy fName))
          fromValue subCon subTree v
  fromObject (p, Prim _ nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = obj .: T.pack (symbolVal (Proxy :: Proxy fName))

instance ( FromObject (Parser p, JsonTree o p rest d) o
         , FromObject (Parser sp, JsonTree a sp sub sd) a
         )
  => FromObject ( Parser (Maybe a -> p)
                , JsonTree o (Maybe a -> p)
                             ((fName .= '( 'Nullable, a, sd, sp, sub)) ': rest)
                             ('S d)
                ) o where
  fromObject (p, Optional _ subCon subTree nxt) obj =
    fromObject (p <*> parser, nxt) obj
      where
        parser = do
          mbV <- obj .:? T.pack (symbolVal (Proxy :: Proxy fName))
          for mbV $ \v ->
            fromValue subCon subTree v


--instance FromObject (Parser c, JsonTree o c fields 'False) o
--  => FromObject (JsonTree o c fields 'True) o where
--    fromObject (CompleteObj name con rest) = fromObject (pure @Parser con, rest)
--
--instance (FromObject (Parser p, JsonTree o p rest 'False) o, FromObject (JsonTree a sp sub 'True) a, FromObject (Parser sp, JsonTree a sp sub 'False) a)
--  => FromObject (Parser (a -> p), JsonTree o (a -> p) ((fName .= '(a, sub)) ': rest) 'False) o
--  where
--    fromObject (p, SubObj _ subtree@(CompleteObj subName _ _) rest) obj =
--      fromObject (p <*> parser, rest) obj
--        where
--          parser = do
--            v <- obj .: T.pack (symbolVal (Proxy :: Proxy fName))
--            flip (withObject subName) v $ \o ->
--              fromObject subtree o
--    fromObject (p, Optional _ subtree@(CompleteObj subName _ _) rest) obj =
--      fromObject (p <*> parser, rest) obj
--        where
--          parser = do
--            mbV <- obj .:? T.pack (symbolVal (Proxy :: Proxy fName))
--            for mbV $ \v ->
--              flip (withObject subName) v $ \o ->
--                fromObject subtree o
--    fromObject (p, Prim _ rest) obj =
--      fromObject ( p <*> obj .: T.pack (symbolVal (Proxy :: Proxy fName))
--                 , rest
--                 )
--                 obj
--
---- need Nat to be able to be inductive from complete object down to empty object?
--
---- make nullability separate from the field type - like a type level bool flag
--
--fromValue :: () --FromObject (JsonTree o c fields 'True) o
--          => JsonTree o c fields 'True -> Value -> Parser o
--fromValue (CompleteObj name _ rest) =
--  withObject name $ \o -> fromObject rest o



  {-

class GetFieldName f where
  getFieldName :: f -> T.Text

instance KnownSymbol fieldName => GetFieldName (JsonTree o p (fieldName .= v ': rest) 'False) where
  getFieldName _ = T.pack (symbolVal (Proxy :: Proxy fieldName))


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

-}
