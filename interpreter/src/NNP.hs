module NNP where

import Grammar
import GrammarTree
import Prop
import Util

-- Interpret a proper noun as a higher-order proposition
i :: GrammarTree -> (Predicate -> Prop)
i (CategoryNode NNP [] ts) p = p [word $ p1 ts]

patterns :: [[Category]]
patterns = []
