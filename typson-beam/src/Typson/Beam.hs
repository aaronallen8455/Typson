{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Typson.Beam
-- Description : Provides the Beam integration
-- Copyright   : (c) Aaron Allen, 2020
-- Maintainer  : Aaron Allen <aaronallen8455@gmail.com>
-- License     : BSD-style (see the file LICENSE)
-- Stability   : experimental
-- Portability : non-portable
--
--------------------------------------------------------------------------------
module Typson.Beam
  ( jsonPath
  , JNullable(..)
  , nullableJsonb
  , nullableJson
  ) where

import qualified Data.Aeson as Aeson
import           Data.Coerce (Coercible, coerce)
import           Data.Kind (Type)
import           Data.List (foldl')
import qualified Data.List.NonEmpty as NE
import           Data.Maybe (fromMaybe)
import           Data.String (fromString)
import qualified Database.Beam as B
import qualified Database.Beam.Backend.SQL.SQL92 as B (HasSqlValueSyntax)
import qualified Database.Beam.Postgres as B
import qualified Database.PostgreSQL.Simple.FromField as Pg

import           Typson

-- | Use a type-safe JSON path as part of a query.
--
-- @
-- select $ jsonPath (Proxy @("foo" :-> "bar")) fieldSchemaJ
--        . fieldAccessor
--      \<$> all_ someTable
-- @
jsonPath :: ( TypeAtPath o tree path ~ field
            , ReflectPath path
            , B.IsPgJSON json
            , Coercible (json field) (JNullable json' field)
            )
         => proxy (path :: k) -- ^ A path proxy
         -> ObjectTree tree o -- ^ Typson schema
         -> B.QGenExpr ctxt B.Postgres s (json o) -- ^ Column selector
         -> B.QGenExpr ctxt B.Postgres s (JNullable json' field)
jsonPath path _ input = coerce $
  case reflectPath path of
    p NE.:| ps -> foldl' buildPath (buildPath input p) ps
  where
    buildPath p (Key k) = p B.->$ fromString k
    buildPath p (Idx i) = p B.-># fromInteger i

--------------------------------------------------------------------------------
-- Selecting Optional JSON
--------------------------------------------------------------------------------

-- | Wraps a @PgJSON@ or @PgJSONB@, treating deserialization of SQL @NULL@ as
-- json @null@. This is so that if you query for a path that might not exist,
-- i.e. a path into an optional field, then an exception will not be raised
-- when attempting to decode the result as JSON.
newtype JNullable json a = JNullable (json a)
  deriving (Ord, Eq, Show) via json a
  deriving B.IsPgJSON via json

deriving via (json a :: Type) instance (B.HasSqlValueSyntax syn (json a))
  => B.HasSqlValueSyntax syn (JNullable json a)

instance ( Pg.FromField (json a :: Type)
         , B.Typeable (a :: Type)
         , B.Typeable json
         )
  => B.FromBackendRow B.Postgres (JNullable json a)

instance Pg.FromField (json a) => Pg.FromField (JNullable json a) where
  fromField f mbBs = JNullable
                 <$> Pg.fromField f (Just $ fromMaybe "null" mbBs)

--------------------------------------------------------------------------------
-- Schema DataTypes
--------------------------------------------------------------------------------

-- | Declares a nullable @PgJSONB@ field in a migration schema
nullableJsonb :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
              => B.DataType B.Postgres (JNullable B.PgJSONB a)
nullableJsonb = coerce (B.jsonb :: B.DataType B.Postgres (B.PgJSONB a))

-- | Declares a nullable @PgJSON@ field in a migration schema
nullableJson :: forall a. (Aeson.ToJSON a, Aeson.FromJSON a)
             => B.DataType B.Postgres (JNullable B.PgJSON a)
nullableJson = coerce (B.json :: B.DataType B.Postgres (B.PgJSON a))

