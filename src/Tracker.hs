-- | This module contains the internal logic for the budget tracker.
module Tracker
  ( newRule,
    deleteRule,
    allocate,
    deallocate,
    reallocate,
    runAllocation,
    addUnallocated,
    DeallocationError (..),
  )
where

import qualified Tracker.Allocation as Allocation
import qualified Tracker.Rules as Rules
import Tracker.State
import Types

newRule :: Rule -> State -> State
newRule r (State rr a) = State (Rules.insert r rr) a

deleteRule :: RuleName -> State -> State
deleteRule rn (State r a) = State (Rules.delete rn r) a

addUnallocated :: Money -> State -> State
addUnallocated m (State r a) = State r (Allocation.addUnallocated m a)

allocate :: Allocation -> State -> State
allocate a (State r aa) = State r (Allocation.add a aa)

data DeallocationError = KeyDNE

deallocate :: Allocation -> State -> Either DeallocationError State
deallocate ruleAllocation (State rules fa) =
  case Allocation.sub ruleAllocation fa of
    Nothing -> Left KeyDNE
    Just fa2 -> Right $ State rules fa2

reallocate :: Allocation -> Allocation -> State -> Either DeallocationError State
reallocate a1 a2 (State rules fa) =
  case Allocation.reallocate a1 a2 fa of
    Nothing -> Left KeyDNE
    Just fa2 -> Right $ State rules fa2

runAllocation :: State -> State
runAllocation (State r a@(FullAllocation _ u)) =
  State r $ Allocation.merge a $ Allocation.runRules u r
