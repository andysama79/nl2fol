module S where

import Grammar
import GrammarTree
import Prop
import NP
import Util
import VP

-- Interpret a sentence as a formula
i :: GrammarTree -> Prop
i (CategoryNode S [NP, VP] ts) = (NP.i $ p1 ts) (VP.i $ p2 ts)

patterns :: [[Category]]
patterns = [[NP, VP]]
