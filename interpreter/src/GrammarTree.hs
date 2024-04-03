module GrammarTree (GrammarTree (..), prettyPrint) where

import Data.Tree (Tree (..), drawTree)
import Grammar (Category)

data GrammarTree
  = LexemeNode String
  | CategoryNode Category [Category] [GrammarTree]
  deriving (Show)

prettyPrint :: GrammarTree -> IO ()
prettyPrint t = putStrLn $ drawTree $ fromGrammarTreeToTree t

fromGrammarTreeToTree :: GrammarTree -> Tree String
fromGrammarTreeToTree (LexemeNode s) = Node s []
fromGrammarTreeToTree (CategoryNode t _ ts) =
  Node (show t) $
    map fromGrammarTreeToTree ts
