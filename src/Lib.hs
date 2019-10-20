{-# LANGUAGE FlexibleContexts #-}
module Lib
    ( processEntity
    , showEntity
    ) where

import Text.Parsec
import Text.Parsec.Error
import Data.Functor.Identity (Identity)
import AnnotationReader (readEntity, Entity)
import GoEntities (convertEntity)


processEntity :: Stream s Identity Char => s -> Either ParseError Entity
processEntity = parse readEntity ""

showEntity :: Either Text.Parsec.Error.ParseError Entity -> String
showEntity (Left err) = show err
showEntity (Right e) = convertEntity e
