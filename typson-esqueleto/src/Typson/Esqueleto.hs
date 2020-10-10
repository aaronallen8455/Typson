{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Typson.Esqueleto
  ( NullableJSONB(..)
  , PostgreSqlJSON
  , jsonPath
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import           Data.Bifunctor (first)
import           Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.PostgreSQL.JSON as E
import qualified Database.Esqueleto.Internal.Internal as E
import           GHC.Generics (Generic)

import           Typson

jsonPath :: ( TypeAtPath o tree path ~ field
            , ReflectPath path
            , PostgreSqlJSON json
            )
         => proxy path
         -> ObjectTree tree o
         -> E.SqlExpr (E.Value (json o))
         -> E.SqlExpr (E.Value (NullableJSONB field))
jsonPath path _ input =
  case reflectPath path of
    p NE.:| ps -> foldl' buildPath (buildPath input p) ps
  where
    buildPath p (Key k) = p `arrOp` E.val k
    buildPath p (Idx i) = p `arrOp` E.val (fromIntegral i :: Int)
    arrOp = E.unsafeSqlBinOp " -> "

-- | Treats SQL 'NULL' as a JSON 'null'
newtype NullableJSONB a =
  NullableJSONB
    { unNullableJSONB :: a
    } deriving ( Generic
               , Aeson.FromJSON
               , Aeson.ToJSON
               , Foldable
               , Functor
               , Eq
               , Ord
               , Read
               , Show
               , Traversable
               )
      deriving E.PersistFieldSql via (E.JSONB a)

instance (Aeson.FromJSON a, Aeson.ToJSON a)
      => E.PersistField (NullableJSONB a) where
  toPersistValue = E.toPersistValue . E.JSONB . unNullableJSONB
  fromPersistValue pVal = fmap NullableJSONB $ case pVal of
      E.PersistNull -> first T.pack
                     $ Aeson.parseEither Aeson.parseJSON Aeson.Null
      _ -> E.unJSONB <$> E.fromPersistValue pVal

-- | Members of this class are type constructors used to respresent
-- Postgres JSON columns.
class PostgreSqlJSON (json :: * -> *)
instance PostgreSqlJSON NullableJSONB
instance PostgreSqlJSON E.JSONB
