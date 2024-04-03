{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module IO (read, write, ParseRecord (..), OutputRecord (..)) where

import Control.Applicative ((<$>))
import Data.Aeson (FromJSON, ToJSON, decode, parseJSON, toJSON)
import Data.Aeson.Encode.Pretty
  ( Config (..),
    Indent (Spaces),
    NumberFormat (Generic),
    encodePretty',
    keyOrder,
  )
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List (isSuffixOf)
import GHC.Generics (Generic)
import Grammar (Category, fromString)
import GrammarTree (GrammarTree (..))
import System.FilePath (isValid)
import Prelude hiding (read)

jsonConf =
  Config
    { confIndent = Spaces 2,
      confCompare =
        keyOrder
          [ "text",
            "grammar",
            "logic",
            "node",
            "children"
          ],
      confNumFormat = Generic,
      confTrailingNewline = False
    }

read :: (FromJSON a) => String -> IO a
read fp
  | checkFilePath fp = do
      text <- readFile fp
      case (decode . BLU.fromString) text of
        Just record -> return record
        Nothing -> error "Invalid file"
  | otherwise = error "Invalid file"

write :: FilePath -> OutputRecord -> IO ()
write fp record
  | checkFilePath fp =
      writeFile fp $
        BLU.toString $
          encodePretty' jsonConf record
  | otherwise = error "Invalid file"

checkFilePath :: String -> Bool
checkFilePath fp = isValid fp && ".json" `isSuffixOf` fp

data ParseRecord = Parse {input :: String, output :: [GrammarTree]}
  deriving (Show, Generic)

data OutputRecord = Output
  { text :: String,
    grammar :: [GrammarTree],
    logic :: [String]
  }
  deriving (Show, Generic)

-- This one is used for parsing JSON into the data type
data GrammarTreeRecord = Obj {node :: String, children :: [GrammarTreeRecord]}
  deriving (Show, Generic)

-- Converting between the record and the ADT
fromRecord :: GrammarTreeRecord -> GrammarTree
fromRecord Obj {node = w, children = []} = LexemeNode w
fromRecord Obj {node = t, children = ts} =
  CategoryNode
    (fromString t)
    (getCats ts)
    (map fromRecord ts)

getCats :: [GrammarTreeRecord] -> [Category]
getCats (Obj {node = w, children = []} : ts') = getCats ts'
getCats (Obj {node = t, children = ts} : ts') = fromString t : getCats ts'
getCats [] = []

toRecord :: GrammarTree -> GrammarTreeRecord
toRecord (LexemeNode w) = Obj {node = w, children = []}
toRecord (CategoryNode cat _ nodes) =
  Obj
    { node = show cat,
      children = map toRecord nodes
    }

instance FromJSON GrammarTreeRecord

instance FromJSON GrammarTree where
  parseJSON json = fromRecord <$> parseJSON json

instance FromJSON ParseRecord

instance FromJSON OutputRecord

instance ToJSON GrammarTreeRecord

instance ToJSON GrammarTree where
  toJSON tree = toJSON $ toRecord tree

instance ToJSON OutputRecord
