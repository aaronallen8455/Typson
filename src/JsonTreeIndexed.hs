{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module JsonTreeIndexed where

import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data JsonTree key typ
  = TreeField key Nullability typ [JsonTree key typ]

type Tree = [JsonTree Symbol Type]

data Nullability
  = Nullable
  | NonNullable

class ObjectSYM repr where
  object :: String -> IFreeAp (Field repr o) t o -> repr t o

newtype ObjectEncoder (t :: Tree) o =
  ObjectEncoder { unObjectEncoder :: o -> Aeson.Value }

newtype ObjectDecoder (t :: Tree) o =
  ObjectDecoder { unObjectDecoder :: Aeson.Value -> Aeson.Parser o }

newtype ObjectTree (t :: Tree) o =
  ObjectTree { getObjectTree :: Proxy t }

instance ObjectSYM ObjectEncoder where
  object _ fields = ObjectEncoder $ \o ->
    Aeson.Object $ runApp_ (`unFieldEncoder` o) fields

instance ObjectSYM ObjectDecoder where
  object name fields = ObjectDecoder . Aeson.withObject name $ \obj ->
    runApp (`unFieldDecoder` obj) fields

instance ObjectSYM ObjectTree where
  object _ _ = ObjectTree Proxy

class FieldSYM repr where
  data Field repr :: Type -> [JsonTree Symbol Type] -> Type -> Type

  prim :: forall key obj tree field.
          ( FromJSON field
          , ToJSON field
          , KnownSymbol key
          , tree ~ '[ 'TreeField key 'NonNullable field '[]]
          )
       => Proxy key
       -> (obj -> field)
       -> Field repr obj tree field

  subObj :: forall key obj tree subTree field.
            ( KnownSymbol key
            , tree ~ '[ 'TreeField key 'NonNullable field subTree]
            )
         => Proxy key
         -> (obj -> field)
         -> repr subTree field
         -> Field repr obj tree field

data IFreeAp f (t :: [JsonTree Symbol Type]) (a :: Type) where
  Pure :: a -> IFreeAp f '[] a
  Ap :: IFreeAp f t (a -> b)
     -> f '[st] a
     -> IFreeAp f (st ': t) b
infixl 3 `Ap`

data IProxy (o :: Type) (i :: [JsonTree Symbol Type]) (a :: Type) = IProxy

instance FieldSYM ObjectTree where
  data Field ObjectTree o t a = FieldProxy
  prim _ _ = FieldProxy
  subObj _ _ _ = FieldProxy

instance FieldSYM ObjectEncoder where
  newtype Field ObjectEncoder o t a =
    FieldEncoder { unFieldEncoder :: o -> Aeson.Object }
  prim key acc = FieldEncoder $ \o -> T.pack (symbolVal key) .= acc o
  subObj key acc (ObjectEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal key) .= so (acc o)

instance FieldSYM ObjectDecoder where
  newtype Field ObjectDecoder o t a =
    FieldDecoder { unFieldDecoder :: Aeson.Object -> Aeson.Parser a }
  prim key _ = FieldDecoder $ \obj ->
    obj .: T.pack (symbolVal key)
  subObj key _ (ObjectDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal key)
    d (Aeson.Object so)

runApp_ :: Monoid m => (forall a t. f t a -> m) -> IFreeAp f t a -> m
runApp_ f (Pure _) = mempty
runApp_ f (Ap p c) = runApp_ f p <> f c

runApp :: Applicative g => (forall a t. f t a -> g a) -> IFreeAp f t a -> g a
runApp _ (Pure a) = pure a
runApp f (Ap p c) = runApp f p <*> f c

data Test =
  Test
    { one :: Bool
    , two :: String
    , three :: Int
    }

test :: (ObjectSYM repr, FieldSYM repr) => repr TestTree Test
test = object "Test"
     $ Pure Test
  `Ap` prim (Proxy :: Proxy "one") one
  `Ap` prim (Proxy :: Proxy "two") two
  `Ap` prim (Proxy :: Proxy "three") three

type TestTree = '[ 'TreeField "three" 'NonNullable Int '[],
                   'TreeField "two" 'NonNullable String '[],
                   'TreeField "one" 'NonNullable Bool '[]]

instance FromJSON Test where
  parseJSON = unObjectDecoder test

instance ToJSON Test where
  toJSON = unObjectEncoder test

data Test2 =
  Test2
    { t2a :: Test
    , t2b :: Bool
    }

test2 :: (ObjectSYM repr, FieldSYM repr) => repr Test2Tree Test2
test2 = object "Test2"
      $ Pure Test2
   `Ap` subObj (Proxy :: Proxy "t2a") t2a test
   `Ap` prim (Proxy :: Proxy "t2b") t2b

type Test2Tree = '[ 'TreeField "t2b" 'NonNullable Bool '[],
                    'TreeField "t2a" 'NonNullable Test TestTree]
