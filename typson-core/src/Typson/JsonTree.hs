{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- for the custom type error
module Typson.JsonTree
  ( JsonTree
  , type Tree(..)
  , type Edge(..)
  , type Aggregator(..)
  , type Multiplicity(..)
  , ObjectSYM(..)
  , ObjectTree
  , FieldSYM(..)
  , UnionSYM(..)
  , NonRecursive
  , NoDuplicateKeys
  , IFreeAp
  , encodeObject
  , decodeObject
  , getObjectTree
  , key
  , (<<$>)
  , (<<*>)
  , runAp
  , runAp_
  ) where

import           Control.Applicative ((<|>))
import           Data.Aeson ((.:), (.:?), (.=), FromJSON, ToJSON)
import qualified Data.Aeson.Types as Aeson
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

-- | This is the type used to represent the JSON form of a haskell type. It is
-- only used at the type level via the @DataKinds@ extension.
data Tree = Node Aggregator [Edge] -- Invariant: [Edge] is non-empty
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

data Multiplicity
  = List      -- ^ Corresponds to a JSON array
  | Singleton -- ^ A non-null field
  | Nullable  -- ^ A field that can be @null@

--------------------------------------------------------------------------------
-- Final-tagless "Symantics" for Object Construction
--------------------------------------------------------------------------------

-- | Used to interpret JSON trees for haskell record types.
class FieldSYM repr => ObjectSYM (repr :: Tree -> Type -> Type) where
  object :: ( t ~ 'Node 'Product edges
            , NonRecursive '[o] edges
            , NoDuplicateKeys o edges
            )
         => String -> IFreeAp (Field repr o) t o -> repr t o

  prim :: ( FromJSON v
          , ToJSON v
          )
       => repr 'Leaf v

class FieldSYM repr where
  data Field repr :: Type -> Tree -> Type -> Type

  field :: ( KnownSymbol key
           , tree ~ 'Node 'Product '[ 'Edge key 'Singleton field subTree]
           )
        => proxy key
        -> (obj -> field)
        -> repr subTree field
        -> Field repr obj tree field

  optField :: ( KnownSymbol key
              , tree ~ 'Node 'Product '[ 'Edge key 'Nullable field subTree]
              )
           => proxy key
           -> (obj -> Maybe field)
           -> repr subTree field
           -> Field repr obj tree (Maybe field)

  optFieldDef :: ( KnownSymbol key
                 , tree ~ 'Node 'Product '[ 'Edge key 'Singleton field subTree]
                 )
              => proxy key
              -> (obj -> field)
              -> field
              -> repr subTree field
              -> Field repr obj tree field
  optFieldDef p getter _ sub = field p getter sub

  listField :: ( KnownSymbol key
               , tree ~ 'Node 'Product '[ 'Edge key 'List field subTree]
               )
            => proxy key
            -> (obj -> [field])
            -> repr subTree field
            -> Field repr obj tree [field]

-- | Used to interpret JSON trees for haskell sum types.
class UnionSYM (repr :: Tree -> Type -> Type) where
  type Result repr union :: Type
  data Tag repr :: Type -> Tree -> Type -> Type

  union :: ( tree ~ 'Node 'Sum edges
           , NonRecursive '[union] edges
           , NoDuplicateKeys union edges
           )
        => String
        -> IFreeAp (Tag repr union) tree (union -> Result repr union)
        -> repr tree union

  tag :: ( KnownSymbol name
         , tree ~ 'Node 'Sum '[ 'Edge name 'Nullable v subTree]
         )
      => proxy name
      -> (v -> union)
      -> repr subTree v
      -> Tag repr union tree (v -> Result repr union)

type JsonTree t a = forall repr. (ObjectSYM repr, UnionSYM repr) => repr t a

key :: Proxy (key :: Symbol)
key = Proxy

--------------------------------------------------------------------------------
-- Implementations
--------------------------------------------------------------------------------

-- | Use a 'Tree' to encode a type as an Aeson 'Value'
newtype ObjectEncoder (t :: Tree) o =
  ObjectEncoder { encodeObject :: o -> Aeson.Value }

-- | Use a 'Tree' to decode a type from an Aeson 'Value'
newtype ObjectDecoder (t :: Tree) o =
  ObjectDecoder { decodeObject :: Aeson.Value -> Aeson.Parser o }

data TreeProxy (t :: Tree) o = TreeProxy

-- | Used to pass a 'Tree' around at the value level.
newtype ObjectTree (t :: Tree) o =
  ObjectTree { getObjectTree :: TreeProxy t o }

instance ObjectSYM ObjectEncoder where
  object _ fields = ObjectEncoder $ \o ->
    Aeson.Object $ runAp_ (`unFieldEncoder` o) fields
  prim = ObjectEncoder Aeson.toJSON

instance ObjectSYM ObjectDecoder where
  object name fields = ObjectDecoder . Aeson.withObject name $ \obj ->
    runAp (`unFieldDecoder` obj) fields
  prim = ObjectDecoder Aeson.parseJSON

instance ObjectSYM ObjectTree where
  object _ _ = ObjectTree TreeProxy
  prim = ObjectTree TreeProxy

instance FieldSYM ObjectTree where
  data Field ObjectTree o t a = FieldProxy
  field _ _ _ = FieldProxy
  optField _ _ _ = FieldProxy
  listField _ _ _ = FieldProxy

instance FieldSYM ObjectEncoder where
  newtype Field ObjectEncoder o t a =
    FieldEncoder { unFieldEncoder :: o -> Aeson.Object }
  field ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= so (acc o)
  optField ky acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal ky) .= (so <$> acc o)
  listField ky acc (ObjectEncoder so) =
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
  listField ky _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal ky)
    traverse d so

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
                tagDecoder v <|> nxt
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

