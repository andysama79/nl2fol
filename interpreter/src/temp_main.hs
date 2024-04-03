module Main where

import Prelude hiding (read)
import System.Environment(getArgs)
import System.FilePath(takeDirectory, takeFileName, (</>))

import IO (read, write, ParseRecord(..), OutputRecord(..))
import GrammarTree(prettyPrint)
import Interpretation(interpret)

main :: IO ()
main = do
    args <- getArgs
    let fp = head args
    records <- read fp

    putStrLn $ records