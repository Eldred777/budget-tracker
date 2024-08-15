module Tracker.State
  ( blankState,
    showState,
    State (..),
  )
where

import Data.List (intercalate)
import qualified Data.Map as Map
import Tracker.Allocation (showAllocation)
import Types (FullAllocation (..), Rules)
import Tracker.Rules (showRules)

data State = State Rules FullAllocation

blankState :: State
blankState = State Map.empty (FullAllocation Map.empty 0)

showState :: State -> String
showState (State r a) = intercalate "\n" ["Current state:", showRules r, showAllocation a]
