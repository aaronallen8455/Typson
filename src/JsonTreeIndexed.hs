{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module JsonTreeIndexed
  ( FieldSYM
  ) where

import           Data.Aeson (FromJSON, ToJSON, (.=), (.:))
import qualified Data.Aeson.Types as Aeson
import           Data.Kind (Type)
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import           GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

data JsonTree key typ
  = TreeField key Nullability typ [JsonTree key typ]

data Nullability
  = Nullable
  | NonNullable

class ObjectSYM repr where
  object :: String -> IFreeAp (Field repr o) t o -> repr t o

class FieldSYM repr where
  --data Field repr :: Type -> JsonTreeNode Symbol Type -> Type -> Type

  prim :: forall key obj tree field.
          ( FromJSON field
          , ToJSON field
          , KnownSymbol key
          , tree ~ '[ 'TreeField key 'NonNullable field '[]]
          )
       => Proxy key
       -> (obj -> field)
       -> repr obj tree field

  subObj :: forall key obj tree subTree field.
            ( KnownSymbol key
            , tree ~ '[ 'TreeField key 'NonNullable field subTree]
            )
         => Proxy key
         -> (obj -> field)
         -> IFreeAp (repr field) subTree field
         -> repr obj tree field

data IFreeAp f (t :: [JsonTree Symbol Type]) (a :: Type) where
  Pure :: a -> IFreeAp f '[] a
  Ap :: IFreeAp f t (a -> b)
     -> f '[st] a
     -> IFreeAp f (st ': t) b
infixl 3 `Ap`

data IProxy (o :: Type) (i :: [JsonTree Symbol Type]) (a :: Type) = IProxy

instance FieldSYM IProxy where
  prim _ _ = IProxy
  subObj _ _ _ = IProxy

newtype FieldEncoder (o :: Type) (t :: [JsonTree Symbol Type]) (a :: Type) =
  FieldEncoder { unFieldEncoder :: o -> Aeson.Object }

instance FieldSYM FieldEncoder where
  prim key acc = FieldEncoder $ \o -> T.pack (symbolVal key) .= acc o
  subObj key acc (FieldEncoder so) =
    FieldEncoder $ \o -> T.pack (symbolVal key) .= so (acc o)

newtype FieldDecoder (o :: Type) (t :: [JsonTree Symbol Type]) (a :: Type) =
  FieldDecoder { unFieldDecoder :: Aeson.Object -> Aeson.Parser a }

instance FieldSYM FieldDecoder where
  prim key _ = FieldDecoder $ \obj ->
    obj .: T.pack (symbolVal key)
  subObj key _ (FieldDecoder d) = FieldDecoder $ \obj -> do
    so <- obj .: T.pack (symbolVal key)
    d so

runApp_ :: Monoid m => (forall a t. f t a -> m) -> IFreeAp f t a -> m
runApp_ f (Pure _) = mempty
runApp_ f (Ap p c) = runApp_ f p <> f c

runApp :: Applicative g => (forall a t. f t a -> g a) -> IFreeAp f t a -> g a
runApp _ (Pure a) = pure a
runApp f (Ap p c) = runApp f p <*> f c

fromValue :: IFreeAp (FieldDecoder a) t a
          -> String
          -> Aeson.Value
          -> Aeson.Parser a
fromValue decoder objName = Aeson.withObject objName $ \obj ->
  runApp (`unFieldDecoder` obj) decoder

toValue :: IFreeAp (FieldEncoder a) t a
        -> a
        -> Aeson.Value
toValue encoder o = Aeson.Object $ runApp_ (`unFieldEncoder` o) encoder

data Test =
  Test
    { one :: Bool
    , two :: String
    , three :: Int
    }

test :: FieldSYM repr => IFreeAp (repr Test) TestTree Test
test = Pure Test
  `Ap` prim (Proxy :: Proxy "one") one
  `Ap` prim (Proxy :: Proxy "two") two
  `Ap` prim (Proxy :: Proxy "three") three

type TestTree = '[ 'TreeField "three" 'NonNullable Int '[],
                   'TreeField "two" 'NonNullable String '[],
                   'TreeField "one" 'NonNullable Bool '[]]

instance FromJSON Test where
  parseJSON = fromValue test "Test"

instance ToJSON Test where
  toJSON = toValue test

data Test2 =
  Test2
    { t2a :: Test
    , t2b :: Bool
    }

test2 :: FieldSYM repr => IFreeAp (repr Test2) Test2Tree Test2
test2 = Pure Test2
   `Ap` subObj (Proxy :: Proxy "t2a") t2a test
   `Ap` prim (Proxy :: Proxy "t2b") t2b

type Test2Tree = '[ 'TreeField "t2b" 'NonNullable Bool '[],
                    'TreeField "t2a" 'NonNullable Test TestTree]
