{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Typson.Beam
  ( JNullable(..)
  , nullableJsonb
  , nullableJson
  , jsonPath
  ) where

import qualified Data.Aeson as Aeson
import           Data.Coerce (coerce)
import           Data.List (foldl')
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL.SQL92 as B (HasSqlValueSyntax)
import qualified Database.Beam.Postgres as B
import qualified Database.PostgreSQL.Simple.FromField as Pg

import           Typson

-- | Construct a type safe JSON path.
jsonPath :: ( TypeAtPath o tree path ~ field
            , ReflectPath path
            , B.IsPgJSON json
            )
         => proxy path
         -> repr tree o
         -> B.QGenExpr ctxt B.Postgres s (json o)
         -> B.QGenExpr ctxt B.Postgres s (json field)
jsonPath path _ input =
  case reflectPath path of
    [] -> error "Should not have an empty path"
    p:ps -> foldl' buildPath (buildPath input p) ps
  where
    buildPath p (Key k) = p B.->$ fromString k
    buildPath p (Idx i) = p B.-># fromInteger i

--------------------------------------------------------------------------------
-- Selecting Optional JSON
--------------------------------------------------------------------------------

-- | Treats deserializing SQL 'NULL' as json 'null'
newtype JNullable json a = JNullable (json a)
  deriving (Ord, Eq, Show) via json a
  deriving B.IsPgJSON via json

deriving via (json a) instance (B.HasSqlValueSyntax syn (json a))
  => B.HasSqlValueSyntax syn (JNullable json a)

instance (Pg.FromField (json a), B.Typeable a, B.Typeable json)
  => B.FromBackendRow B.Postgres (JNullable json a)

instance Pg.FromField (json a) => Pg.FromField (JNullable json a) where
  fromField field mbBs = JNullable
                     <$> Pg.fromField field (Just $ fromMaybe "null" mbBs)

--------------------------------------------------------------------------------
-- Schema DataTypes
--------------------------------------------------------------------------------

nullableJsonb :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
              => B.DataType B.Postgres (JNullable B.PgJSONB a)
nullableJsonb = coerce (B.jsonb :: B.DataType B.Postgres (B.PgJSONB a))

nullableJson :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
             => B.DataType B.Postgres (JNullable B.PgJSON a)
nullableJson = coerce (B.json :: B.DataType B.Postgres (B.PgJSON a))

