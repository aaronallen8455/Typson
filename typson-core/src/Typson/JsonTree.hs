{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for the custom type error
--------------------------------------------------------------------------------
-- |
-- Module      : Typson.JsonTree
-- Description : Provides the core type classes and data structures for JSON
--   representation
-- Copyright   : (c) Aaron Allen, 2020
-- Maintainer  : Aaron Allen <aaronallen8455@gmail.com>
-- License     : BSD-style (see the file LICENSE)
-- Stability   : experimental
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Typson.JsonTree
  ( -- * Schema Semantics
    -- | Type classes and type-level data structures for representing the
    -- JSON structure of data.

    -- ** Defining JSON Schemas
    ObjectSYM(..)
  , FieldSYM(..)
  , UnionSYM(..)
  , JsonSchema
  , key
    -- ** Core Interpreters
    -- | A single schema can be interpreted in different ways. This allows it to
    -- be used as both an encoder and decoder.
    -- Because the schema semantics are using the final tagless style, users are
    -- able to write their own interpreters.
  , ObjectEncoder(..)
  , ObjectDecoder(..)
  , ObjectTree(..)
  -- ** Specialized Indexed Free Applicative
  , IFreeAp
  , (<<$>)
  , (<<*>)
  , runAp
  , runAp_
    -- ** Core Data Structure
  , type Tree(..)
  , type Edge(..)
  , type Aggregator(..)
  , type Multiplicity(..)
  , NonRecursive
  , NoDuplicateKeys
  ) where

import           Data.Aeson ((.:), (.:?), (.=), FromJSON, ToJSON)
import qualified Data.Aeson.Types as Aeson
import           Data.Foldable (toList)
import           Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Strict as HM
import           Data.Kind (Constraint, Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           Data.Type.Bool (If)
import           GHC.TypeLits (ErrorMessage(..), KnownSymbol, Symbol, TypeError, symbolVal)

--------------------------------------------------------------------------------
-- Type-level JSON Tree Representation
--------------------------------------------------------------------------------

-- | This is the data structure used to represent the JSON form of a haskell type. It is
-- only used at the type level via the @DataKinds@ extension. You shouldn't write
-- this type yourself, instead it's recommended that you let the compiler derive
-- it for you using the @PartialTypeSignatures@ extension and turning off warnings
-- for partial signatures using @-fno-warn-partial-type-signatures@. The 'Tree'
-- argument can then be replaced with @_@ in the type signature of your schemas:
--
-- @
--    personJ :: JsonSchema _ Person
-- @
data Tree = Node Aggregator [Edge] -- Invariant: [Edge] is non-empty
          | ListNode Tree
          | Leaf

data Edge
  = Edge
      Symbol       -- ^ The json field key
      Multiplicity -- ^ The multiplicity of the field's value
      Type         -- ^ The type of the value at the key
      Tree         -- ^ 'Tree' for the value's type

data Aggregator
  = Product -- ^ Object has all fields from a list
  | Sum     -- ^ Object has exactly one field from a list of possible fields
  -- need one for arrays and maps?

data Multiplicity
  = Singleton -- ^ A non-null field
  | Nullable  -- ^ A field that can be @null@

--------------------------------------------------------------------------------
-- Final-tagless "Symantics" for Object Construction
--------------------------------------------------------------------------------

-- | Used to interpret JSON trees for haskell record types.
class FieldSYM repr => ObjectSYM (repr :: Tree -> Type -> Type) where
  -- | Declares the schema for a record type.
  --
  -- @
  --    data Person =
  --      Person
  --        { name :: Text
  --        , age  :: Int
  --        }
  --
  --    personJ :: JsonSchema _ Person
  --    personJ = object "Person" $
  --      Person
  --        \<\<$> field (key @"name") name prim
  --        \<\<*> field (key @"age") age prim
  -- @
  object :: ( t ~ 'Node 'Product edges
            , NonRecursive '[o] edges
            , NoDuplicateKeys o edges
            )
         => String -- ^ Name of the object as it will appear in parse errors
         -> IFreeAp (Field repr o) t o -- ^ The collection of fields
         -> repr t o

  -- | Serves as a schema for a type that cannot itself be broken down into
  -- named fields. The type must have 'FromJSON' and 'ToJSON' instances.
  prim :: ( FromJSON v
          , ToJSON v
          )
       => repr 'Leaf v

  list :: repr t o
       -> repr ('ListNode t) [o]

class FieldSYM repr where
  data Field repr :: Type -> Tree -> Type -> Type

  -- | Defines a required field
  field :: ( KnownSymbol key
           , edge ~ 'Edge key 'Singleton field subTree
           , tree ~ 'Node 'Product '[edge]
           )
        => proxy key -- ^ The 'Symbol' to use as the key in the JSON object
        -> (obj -> field) -- ^ The accessor for the field
        -> repr subTree field -- ^ Schema for the type of the field
        -> Field repr obj tree field

  -- | Defines an optional field. Will parse 'Nothing' for either a @null@ JSON
  -- value or if the key is missing. Will encode 'Nothing' as @null@.
  optField :: ( KnownSymbol key
              , edge ~ 'Edge key 'Nullable field subTree
              , tree ~ 'Node 'Product '[edge]
              )
           => proxy key -- ^ The 'Symbol' to use as the key in the JSON object
           -> (obj -> Maybe field) -- ^ The accessor for the field
           -> repr subTree field -- ^ Schema for the type of the field
           -> Field repr obj tree (Maybe field)

  -- | Defines an optional field where parsing will emit the given default value
  -- if the field is @null@ or the key is absent.
  optFieldDef :: ( KnownSymbol key
                 , edge ~ 'Edge key 'Singleton field subTree
                 , tree ~ 'Node 'Product '[edge]
                 )
              => proxy key -- ^ The 'Symbol' to use as the key in the JSON object
              -> (obj -> field) -- ^ The accessor for the field
              -> field -- ^ Default value to emit
              -> repr subTree field -- ^ Schema for the type of the field
              -> Field repr obj tree field
  optFieldDef p getter _ sub = field p getter sub

-- | Used to interpret JSON trees for haskell sum types.
class UnionSYM (repr :: Tree -> Type -> Type) where
  type Result repr union :: Type
  data Tag repr :: Type -> Tree -> Type -> Type

  -- | Declares a schema for a tagged sum type
  --
  -- @
  --    data Classifier
  --      = Flora Plant
  --      | Fauna Animal
  --
  --    classifierJ :: JsonSchema _ Classifier
  --    classifierJ = union "Classifier" $
  --      classifierTags
  --        <<$> tag (key @"flora") Flora plantJ
  --        <<*> tag (key @"fauna") Fauna fuanaJ
  -- @
  union :: ( tree ~ 'Node 'Sum edges
           , NonRecursive '[union] edges
           , NoDuplicateKeys union edges
           )
        => String -- ^ The 'Symbol' to use as the key in the JSON object
        -> IFreeAp (Tag repr union) tree (union -> Result repr union)
           -- ^ A collection of tags, one for each branch
        -> repr tree union

  -- | Used to declare a single branch of a sum type. The constructor for the
  -- branch should take a single argument. If you require more than one argument
  -- then you should package them up into a separate record type.
  --
  -- The resulting JSON is an object with a single field with a key/value pair
  -- corresponding to one of the branches of the sum type.
  tag :: ( KnownSymbol name
         , edge ~ 'Edge name 'Nullable v subTree
         , tree ~ 'Node 'Sum '[edge]
         )
      => proxy name -- ^ 'Symbol' used as the JSON key for the field
      -> (v -> union) -- ^ Data constructor
      -> repr subTree v -- ^ Schema for the value that this branch tags
      -> Tag repr union tree (v -> Result repr union)

-- | A rank-N type synonym used in the type signature of JSON schemas
type JsonSchema t a = forall repr. (ObjectSYM repr, UnionSYM repr) => repr t a

-- | A synonym for 'Proxy' that takes a 'Symbol'. Intended to be used in 'field'
-- and 'tag' definitions.
key :: Proxy (key :: Symbol)
key = Proxy

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

-- | Use a 'Tree' to encode a type as an Aeson 'Value'
newtype ObjectEncoder (t :: Tree) o =
  ObjectEncoder
    { -- | Uses a schema as a JSON encoder
      --
      -- @
      --    instance ToJSON Person where
      --      toJSON = encodeObject personJ
      -- @
      encodeObject :: o -> Aeson.Value
    }

-- | Use a 'Tree' to decode a type from an Aeson 'Value'
newtype ObjectDecoder (t :: Tree) o =
  ObjectDecoder
    { -- | Uses a schema as a JSON parser
      --
      -- @
      --    instance FromJSON Person where
      --      parseJSON = decodeObject personJ
      -- @
      decodeObject :: Aeson.Value -> Aeson.Parser o
    }

data TreeProxy (t :: Tree) o = TreeProxy

-- | Used to pass a 'Tree' around at the value level.
newtype ObjectTree (t :: Tree) o =
  ObjectTree { getObjectTree :: TreeProxy t o }

instance ObjectSYM ObjectEncoder where
  object _ fields = ObjectEncoder $ \o ->
    Aeson.Object $ runAp_ (`unFieldEncoder` o) fields
  list (ObjectEncoder e) = ObjectEncoder $
    Aeson.toJSON . map e
  prim = ObjectEncoder Aeson.toJSON

instance ObjectSYM ObjectDecoder where
  object name fields = ObjectDecoder . Aeson.withObject name $ \obj ->
    runAp (`unFieldDecoder` obj) fields
  list (ObjectDecoder d) = ObjectDecoder . Aeson.withArray "List" $
    traverse d . toList
  prim = ObjectDecoder Aeson.parseJSON

instance ObjectSYM ObjectTree where
  object _ _ = ObjectTree TreeProxy
  list _ = ObjectTree TreeProxy
  prim = ObjectTree TreeProxy

instance FieldSYM ObjectTree where
  data Field ObjectTree o t a = FieldProxy
  field _ _ _ = FieldProxy
  optField _ _ _ = FieldProxy

instance FieldSYM ObjectEncoder where
  newtype Field ObjectEncoder o t a =
    FieldEncoder { unFieldEncoder :: o -> Aeson.Object }
  field ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= so (acc o)
  optField ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= (so <$> acc o)

instance FieldSYM ObjectDecoder where
  newtype Field ObjectDecoder o t a =
    FieldDecoder { unFieldDecoder :: Aeson.Object -> Aeson.Parser a }
  field ky _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal ky)
    d so
  optField ky _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    mbSo <- obj .:? T.pack (symbolVal ky)
    traverse d mbSo
  optFieldDef ky _ def (ObjectDecoder d) = FieldDecoder $ \obj -> do
    mbSo <- obj .:? T.pack (symbolVal ky)
    maybe (pure def) d mbSo

instance UnionSYM ObjectEncoder where
  newtype Tag ObjectEncoder u t a =
    TagEncoder { unTagEncoder :: a }
  type Result ObjectEncoder u = Aeson.Value
  union _ tags = ObjectEncoder $
    runIdentity (runAp (Identity . unTagEncoder) tags)
  tag name _ valueEncoder =
    TagEncoder $ \v ->
      Aeson.object
        [ T.pack (symbolVal name) .= encodeObject valueEncoder v ]

instance UnionSYM ObjectDecoder where
  newtype Tag ObjectDecoder u t a =
    TagDecoder { unTagDecoder :: HM.HashMap T.Text (Aeson.Value -> Aeson.Parser u) }
  type Result ObjectDecoder u = ()
  union name tags = ObjectDecoder .
    Aeson.withObject name $ \obj -> do
      let decoderMap = runAp_ unTagDecoder tags
          decodeVal k v nxt =
            case HM.lookup k decoderMap of
              Nothing -> nxt
              Just tagDecoder ->
                tagDecoder v
      HM.foldrWithKey decodeVal (fail "Unable to find a matching tag") obj
  tag name constr valueDecoder =
    TagDecoder . HM.singleton (T.pack $ symbolVal name)
      $ fmap constr . decodeObject valueDecoder

instance UnionSYM ObjectTree where
  data Tag ObjectTree u t a = TagProxy
  type Result ObjectTree u = ()
  union _ _ = ObjectTree TreeProxy
  tag _ _ _ = TagProxy

--------------------------------------------------------------------------------
-- No Duplicate Keys Constraint
--------------------------------------------------------------------------------

type family NoDuplicateKeys (obj :: Type) (edges :: [Edge]) :: Constraint where
  NoDuplicateKeys obj ('Edge key q ty subTree ': rest)
    = (KeyNotPresent key obj rest, NoDuplicateKeys obj rest)
  NoDuplicateKeys obj '[] = ()

type family KeyNotPresent (key :: Symbol) (obj :: Type) (edges :: [Edge]) :: Constraint where
  KeyNotPresent key obj ('Edge key q ty subTree ': rest)
    = TypeError ('Text "Duplicate JSON key \""
            ':<>: 'Text key
            ':<>: 'Text "\" in object "
            ':<>: 'ShowType obj
                )
  KeyNotPresent key obj ('Edge notKey q ty subTree ': rest)
    = KeyNotPresent key obj rest
  KeyNotPresent key obj '[] = ()

--------------------------------------------------------------------------------
-- No Recursion Constraint
--------------------------------------------------------------------------------

-- TODO How beneficial is this?
type family NonRecursive (visited :: [Type]) (edges :: [Edge]) :: Constraint where
  NonRecursive visited ('Edge key q ty tree ': rest)
    = If (Elem ty visited)
         (TypeError ('Text "Recursive JSON types are not allowed."))
         (NonRecursive visited rest, NonRecursive (ty ': visited) (GetEdges tree))
  NonRecursive visited '[] = ()

type family GetEdges (t :: Tree) :: [Edge] where
  GetEdges ('Node aggr edges) = edges
  GetEdges 'Leaf = '[]
  GetEdges ('ListNode st) = GetEdges st

type family Elem (needle :: Type) (haystack :: [Type]) :: Bool where
  Elem needle (needle ': rest) = 'True
  Elem needle (head ': rest) = Elem needle rest
  Elem needle '[] = 'False

-- TODO can the two constraints be fused?

--------------------------------------------------------------------------------
-- Free Indexed Applicative
--------------------------------------------------------------------------------

-- | An Indexed Free Applicative variant that is used to build 'Tree's by
-- gathering up all the edges.
data IFreeAp (f :: Tree -> Type -> Type) (t :: Tree) (a :: Type) where
  Pure :: a -> IFreeAp f ('Node aggr '[]) a
  Ap   :: IFreeAp f ('Node aggr edges) (a -> b)
       -> f ('Node aggr '[edge]) a
       -> IFreeAp f ('Node aggr (edge ': edges)) b

-- | Intended to be used like '<$>'
(<<$>) :: (a -> b)
       -> f ('Node aggr '[edge]) a
       -> IFreeAp f ('Node aggr '[edge]) b
f <<$> i = Pure f `Ap` i
infixl 4 <<$>

-- | Intended to be used like '<*>'
(<<*>) :: IFreeAp f ('Node aggr edges) (a -> b)
       -> f ('Node aggr '[edge]) a
       -> IFreeAp f ('Node aggr (edge ': edges)) b
(<<*>) = Ap
infixl 4 <<*>

runAp_ :: Monoid m => (forall a' t'. f t' a' -> m) -> IFreeAp f t a -> m
runAp_ _ (Pure _) = mempty
runAp_ f (Ap p c) = runAp_ f p <> f c

runAp :: Applicative g => (forall a' t'. f t' a' -> g a') -> IFreeAp f t a -> g a
runAp _ (Pure a) = pure a
runAp f (Ap p c) = runAp f p <*> f c

