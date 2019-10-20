module Main where

import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)

import Lib (processEntity, showEntity)


main :: IO ()
main = do
    args <- getArgs

    when (length args /= 1) $ do
        putStrLn "input file path"
        filepath <- getLine
        readFileAndProcess filepath

    when (length args == 1) $ readFileAndProcess $ head args 

    putStrLn "Impossible to process the entity"
    exitFailure

readFileAndProcess :: String -> IO ()
readFileAndProcess filepath = do    
    content <- readFile filepath    
    let contentString = showEntity $ processEntity content
    let dest = take (length filepath - 3) filepath ++ "go"
    writeFile dest contentString
    exitSuccess