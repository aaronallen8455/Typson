{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Orville
  ( jsonPathFromSql
  , json
  ) where

import           Control.Monad ((<=<))
import           Data.Aeson (FromJSON, ToJSON, decodeStrict, encode)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy (Proxy(..))
import qualified Data.Text as T
import qualified Database.HDBC as HDBC
import qualified Database.Orville.PostgreSQL as O

import           JsonTree (CollapseMaybes, JsonTree, TypeAtPath, ReflectPath(..))

jsonPathFromSql :: forall path o fields field con d.
                   ( CollapseMaybes (TypeAtPath (JsonTree o con con fields d) path) ~ field
                   , ReflectPath path
                   , FromJSON field
                   , ToJSON field
                   )
                => JsonTree o con con fields d
                -> O.FieldDefinition o
                -> O.FromSql field
jsonPathFromSql _ fieldDef = O.fieldFromSql $ O.fieldOfType json path
  where
    keys = reflectPath (Proxy :: Proxy path)
    path = O.fieldName fieldDef <> " -> " <> T.unpack (buildPath keys)
    buildPath [a, b] = "'" <> a <> "' ->> '" <> b <> "'"
    buildPath [a] = "'" <> a <> "'"
    buildPath (a : rest) = "'" <> a <> "' -> " <> buildPath rest
    buildPath [] = "" -- TODO use non-empty list

json :: (ToJSON a, FromJSON a) => O.SqlType a
json =
  O.SqlType
    { O.sqlTypeDDL = "JSONB"
    , O.sqlTypeReferenceDDL = Nothing
    , O.sqlTypeNullable = False
    , O.sqlTypeId = HDBC.SqlUnknownT "3802"
    , O.sqlTypeSqlSize = Nothing
    , O.sqlTypeToSql = jsonToSql
    , O.sqlTypeFromSql = jsonFromSql
    }

jsonToSql :: ToJSON a => a -> HDBC.SqlValue
jsonToSql = HDBC.SqlByteString . BSL.toStrict . encode

jsonFromSql :: FromJSON a => HDBC.SqlValue -> Maybe a
jsonFromSql = decodeStrict <=< byteStringFromSql

byteStringFromSql :: HDBC.SqlValue -> Maybe BS8.ByteString
byteStringFromSql sql =
  case sql of
    HDBC.SqlByteString bytes -> Just bytes
    HDBC.SqlString string -> Just $ BS8.pack string
    _ -> Nothing

