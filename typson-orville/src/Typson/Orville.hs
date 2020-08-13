{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Typson.Orville
  ( jsonPathSql
  , json
  , JsonSqlParts(..)
  ) where

import           Control.Monad ((<=<))
import           Data.Aeson (FromJSON, Result(Success), ToJSON, Value(Null), decodeStrict, encode, fromJSON)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Proxy (Proxy(..))
import qualified Database.HDBC as HDBC
import qualified Database.Orville.PostgreSQL as O

import           Typson (ReflectPath(..), TypeAtPath, sqlPath)

data JsonSqlParts field =
  JsonSqlParts
    { selectorString :: String
    , queryPath      :: String
    , deserializer   :: O.FromSql field
    }

jsonPathSql :: ( TypeAtPath o tree path ~ field
               , ReflectPath path
               , FromJSON field -- TODO json constraints needed?
               , ToJSON field
               )
            => proxy path
            -> repr tree o
            -> O.FieldDefinition o
            -> JsonSqlParts field
jsonPathSql pathProxy _ fieldDef =
  JsonSqlParts
    { selectorString = selector
    , queryPath      = path
    , deserializer   = fromSql
    }
  where
    path = O.fieldName fieldDef <> " -> " <> sqlPath pathProxy
    selector = path <> " AS " <> "\"" <> path <> "\""
    fromSql = O.fieldFromSql
            . O.fieldOfType json
            $ path

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
jsonFromSql = handleResult . fromJSON
          <=< valueFromSql
  where
    handleResult (Success a) = Just a
    handleResult _           = Nothing

valueFromSql :: HDBC.SqlValue -> Maybe Value
valueFromSql sql =
  case sql of
    HDBC.SqlByteString bytes -> decodeStrict bytes
    HDBC.SqlString string -> decodeStrict $ BS8.pack string
    HDBC.SqlNull -> Just Null
    _ -> Nothing

