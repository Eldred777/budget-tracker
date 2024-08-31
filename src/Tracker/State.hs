module Tracker.State
  ( blankState,
    showState,
    State (..),
    clearAllocations,
    clearRules,
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import Tracker.Allocation (showAllocation)
import qualified Tracker.Allocation as Allocation
import Tracker.Rules (showRules)
import qualified Tracker.Rules as Rules
import Types (FullAllocation (..), Rules)

data State = State Rules FullAllocation

blankState :: State
blankState = State Map.empty (FullAllocation Map.empty 0)

showState :: State -> String
showState (State r a) = intercalate "\n" [showRules r, showAllocation a]

clearAllocations, clearRules :: State -> State
clearAllocations (State rules _) = State rules Allocation.emptyAllocation
clearRules (State _ a) = State Rules.emptyRules Allocation.emptyAllocation
