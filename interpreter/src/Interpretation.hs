module Interpretation (interpret) where

import Grammar (Category (ROOT, S))
import GrammarTree (GrammarTree (..))
import Prop
import S

interpret :: GrammarTree -> Prop
interpret (CategoryNode ROOT [S] [t]) = S.i t
interpret _ = error "Interpretation must start from ROOT and contain a sentence"
