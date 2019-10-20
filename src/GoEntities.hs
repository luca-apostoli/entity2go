{-# LANGUAGE OverloadedStrings #-}
module GoEntities (
    convertEntity
) where

import AnnotationReader (Entity(..), Property(..), Type(..))
import Data.List
import Data.Char
import Data.Maybe


convertEntity :: Entity -> String
convertEntity e = "type " ++ entityName e ++ " struct{\n" ++ prettyPrintProperties (properties e) ++ "\n}"



prettyPrintProperties :: [Property] -> String
prettyPrintProperties ps = unlines $ map printProperty $ filter (isNothing . exclude) ps
                            where                                
                                printProperty p = "\t" ++ upperFirst (name p) 
                                                    ++ "\t\t\t\t" ++ convertType (ptype p) 
                                                    ++ "\t\t\t\t" ++ "`json:\"" ++ getName p ++ ",omitempty\"`"
                                upperFirst (c:cs) = toUpper c : cs

getName :: Property -> String
getName p = case serName p of 
                Just n -> n
                _ -> name p


convertType :: Type -> String
convertType PropInteger = "int32"
convertType PropString = "string"
convertType PropFloat = "float32"
convertType PropDateTime = "time.Time"
convertType _ = "string"