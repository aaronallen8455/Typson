{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}
module Typson.Selda
  ( jsonPath
  , Json(..)
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import           Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable (Typeable)
import qualified Database.Selda as S
import qualified Database.Selda.Backend as S
import           Database.Selda.JSON ()
import qualified Database.Selda.PostgreSQL as S
import qualified Database.Selda.Unsafe as S

import           Typson

jsonPath :: ( TypeAtPath o tree path ~ target
            , ReflectPath path
            )
         => proxy (path :: k)
         -> ObjectTree tree o
         -> S.Col S.PG (Json o)
         -> S.Col S.PG (Json target)
jsonPath path _ col =
  case reflectPath path of
    p NE.:| ps -> foldl' buildPath (buildPath col p) ps
  where
    buildPath c (Key k) = S.operator "->" c (fromString k :: S.Col S.PG T.Text)
    buildPath c (Idx i) = S.operator "->" c (S.rawExp (T.pack $ show i) :: S.Col S.PG Int)
    -- had to resort to `rawExp` here because selda uses bigint for Int which
    -- does not work with the -> operator

--------------------------------------------------------------------------------
-- Json Serialization Wrapper
--------------------------------------------------------------------------------

-- | Use this wrapper on fields that are serialized as JSON in the database.
newtype Json a =
  Json
    { unJson :: a
    } deriving (Show, Eq, Ord)
      deriving newtype (Aeson.ToJSON, Aeson.FromJSON)

decodeError :: Show a => a -> b
decodeError x = error $ "fromSql: json column with invalid json: " ++ show x

typeError :: Show a => a -> b
typeError x = error $ "fromSql: json column with non-text value: " ++ show x

instance (Typeable a, Aeson.ToJSON a, Aeson.FromJSON a, Show a) => S.SqlType (Json a) where
  mkLit j =
    case S.mkLit $ Aeson.toJSON j of
      S.LCustom rep l -> S.LCustom rep l
  sqlType _ = S.TJSON
  defaultValue =
    case S.mkLit Aeson.Null of
      S.LCustom rep l -> S.LCustom rep l
  fromSql (S.SqlBlob t) =
    fromMaybe (decodeError t) (Aeson.decode' $ BSL.fromStrict t)
  fromSql (S.SqlString t) =
    fromMaybe (decodeError t) (Aeson.decode' . BSL.fromStrict $ TE.encodeUtf8 t)
  fromSql S.SqlNull =
    case Aeson.fromJSON Aeson.Null of
      Aeson.Success a -> a
      _ -> typeError S.SqlNull
  fromSql x = typeError x

