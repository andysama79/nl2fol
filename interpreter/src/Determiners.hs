module Determiners (quantifier, Quantifier (..), table) where

import Data.Char (toLower)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Prop as Logic

-- Pattern matching quantifiers
data Quantifier
  = Exists -- Existential
  | ForAll -- Universal
  | NegForAll -- Negated universal quantification
  deriving (Eq)

instance Show Quantifier where
  show qtf = case qtf of
    Exists -> "Existential"
    ForAll -> "Universal"
    NegForAll -> "Negated universal"

-- Map determiners to quantifiers
determiners :: Map String Quantifier
determiners = Map.fromList table

table :: [(String, Quantifier)]
table =
  [ ("a", Exists),
    ("an", Exists),
    ("the", Exists),
    ("every", ForAll),
    ("no", NegForAll)
  ]

toLogic :: Quantifier -> ((Logic.Ind -> Logic.Prop) -> Logic.Prop)
toLogic Exists = Logic.Exists
toLogic ForAll = Logic.ForAll

quantifier :: String -> Quantifier
quantifier determiner = determiners Map.! map toLower determiner
