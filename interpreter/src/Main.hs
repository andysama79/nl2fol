module Main where

import GrammarTree (prettyPrint)
import IO (OutputRecord (..), ParseRecord (..), read, write)
import Interpretation (interpret)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, takeFileName, (</>))
import Prelude hiding (read)

main :: IO ()
main = do
  args <- getArgs
  let fp = head args
  record <- read fp
  putStrLn $ input record
  putStrLn ""
  let trees = output record
  mapM_ prettyPrint trees
  let props = map interpret trees
  mapM_ print props
  let fp' = takeDirectory fp </> ("_" ++ takeFileName fp)
  write
    fp'
    Output
      { text = input record,
        grammar = output record,
        logic = map show props
      }
