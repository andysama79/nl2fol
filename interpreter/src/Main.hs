module Main where

import Prelude hiding (read)
import System.Environment(getArgs)
import System.FilePath(takeDirectory,takeFileName,(</>))

import IO ( read, write, ParseRecord(..), OutputRecord(..) )
import GrammarTree(prettyPrint)
import Interpretation(interpret)

main :: IO ()
main = do
  args <- getArgs
  let fp = head args
  record <- read fp
  putStrLn  $ input record
  putStrLn ""
  let trees = output record
  mapM_ prettyPrint trees
  let props = map interpret trees
  mapM_ print props
  let fp' = takeDirectory fp </> ("_" ++ takeFileName fp)
  putStrLn $ "\nWriting back results to the file " ++ fp' ++ "..."
  write fp' Output{text=input record,
                  grammar=output record,
                  logic=map show props}
