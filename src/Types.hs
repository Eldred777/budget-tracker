module Types
  ( Money,
    RuleName,
    RuleType (..),
    Rule (..),
    Rules,
    Allocation (..),
    Allocations,
    FullAllocation (..),
    showMoney,
  )
where

import Data.Map (Map)
import GHC.Float (int2Float)
import Numeric

-- | Amount of money in cents.
type Money = Int

showMoney :: Money -> String
showMoney x = "$" ++ showFFloat (Just 2) (int2Float x / 100) ""

{-
>>> showMoney 1
>>> showMoney 100
>>> showMoney 10253
"$0.01"
"$1.00"
"$102.53"
-}

type RuleName = String

type AllocationName = RuleName

data RuleType = Fixed Money | Proportion Float

instance Show RuleType where
  show (Fixed x) = showMoney x
  show (Proportion x) = showFFloat (Just 2) (x * 100) "%"

type Rules = Map RuleName RuleType

type Allocations = Map AllocationName Money

-- | First argument represents the allocation to each rule; the second
-- represents the amount unallocated to any rule.
data FullAllocation = FullAllocation Allocations Money deriving (Show, Eq)

-- Intermediate representations constructed by Command.Parse, and consumed by
-- Tracker

-- | Intermediate representation of a rule. This should only ever be constructed
-- by Command.Parse
data Rule = Rule {ruleName :: RuleName, ruleType :: RuleType}

-- | Intermediate representation of a rule allocation. This should only ever be
-- constructed by Command.Parse
data Allocation = Allocation RuleName Money deriving (Show, Eq)
