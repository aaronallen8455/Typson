{-# LANGUAGE TypeFamilies #-}
module Typson.Beam
  ( jsonPath
  ) where

import           Data.Proxy (Proxy(..))
import           Data.String (fromString)
import qualified Database.Beam as B
import qualified Database.Beam.Postgres as B
import qualified Database.Beam.Postgres.Syntax as B

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
jsonPath path _ input = go input $ reflectPath path
  where
    go acc (p : rest) = go (buildPath p acc) rest
    buildPath (Key k) = (B.->$ fromString k)
    buildPath (Idx i) = (B.-># fromInteger i)

