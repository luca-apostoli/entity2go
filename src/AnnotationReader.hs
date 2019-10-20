{-# LANGUAGE FlexibleContexts #-}
module AnnotationReader (
    readEntity
    , Entity(..)
    , Property(..)
    , Type(..)
) where

import Text.Parsec
import Text.Parsec.Char
import Data.Tuple (swap)
import Control.Applicative (liftA2, liftA3)

data Type = PropInteger
            | PropString
            | PropArray
            | PropObject
            | PropFloat
            | PropEnum
            | PropDateTime
            deriving (Show, Eq)


data Property = Property {
        name :: PropertyName
      , serName :: Maybe SerializedName
      , exclude :: Maybe String
      , ptype :: Type

} deriving (Show, Eq)

type PropertyName = String
type SerializedName = String
type SerializedType = String

data Entity = Entity {
        entityName :: String
        , properties :: [Property] 
    } deriving (Show, Eq)

-- reads the whole entity
readEntity :: Stream s m Char => ParsecT s u m Entity
readEntity = liftA2 Entity readEntityName readEntityProperties
            where
                readEntityName = manyTill anyChar (try (string "class" >> spaces))
                                *> many alphaNum
                                <* manyTill anyChar (char '{')


makeProperty :: (SerializedType, Maybe SerializedName, Maybe String, PropertyName) -> Property
makeProperty (stype, sname, excl, pname) = Property pname sname excl (readType stype)


readEntityProperties :: Stream s m Char => ParsecT s u m [Property]
readEntityProperties = map makeProperty <$> 
                            ((,,,) <$> lookForSerializedType <*> lookForSerializedName <*> lookForSerializedExclude <*> readFromPropertyName)
                             `sepBy` (spaces >> string "@var")
                        where
                            lookForSerializedType = manyTill anyChar (try (lookAhead serializedType))
                                                *> serializedType
                                                <* readTillSerializeOrEnd
                            lookForSerializedName = optionMaybe serializedName <* readTillSerializeOrEnd
                            lookForSerializedExclude = optionMaybe serializedExclude
                            readFromPropertyName = manyTill anyChar (try (string "*/")) *>
                                                propertyName <* manyTill anyChar (try (lookAhead (string "@var" <|> string "}")))
                            readTillSerializeOrEnd = manyTill anyChar (try (lookAhead (string "@Serializer\\")) <|> try (lookAhead (string "*/"))) 



-- property name is a single line with accessor followed by dollar sign and then eol
propertyName :: Stream s m Char => ParsecT s u m PropertyName
propertyName = manyTill (space <|> endOfLine) accessor >> spaces *> variable <* eol
            where
                variable = char '$' *> many (alphaNum <|> char '_')

serializedName :: Stream s m Char => ParsecT s u m SerializedName
serializedName = try (string "@Serializer\\SerializedName(\"") *> many (alphaNum <|> char '_') <* string "\")"

serializedExclude :: Stream s m Char => ParsecT s u m String
serializedExclude = try (string "@Serializer\\Exclude") *> string "()"

serializedType ::  Stream s m Char => ParsecT s u m SerializedType
serializedType = string "@Serializer\\Type(\"" *> many (alphaNum <|> char '_')

-- read property types
readType :: String -> Type
readType s = case s of
        "integer" -> PropInteger
        "string" -> PropString
        "array" -> PropArray
        "object" -> PropObject
        "float" -> PropFloat
        "enum" -> PropEnum
        "DateTime" -> PropDateTime
        _ -> PropString

-- The end of line character is ';'
eol :: Stream s m Char => ParsecT s u m Char
eol = char ';'

-- property accessor one of the three possibilities plus spaces (ignored)
accessor :: Stream s m Char => ParsecT s u m String
accessor = try (string "private")
           <|> try (string "public")
           <|> string "protected"
           <?> "Accessor not found"
