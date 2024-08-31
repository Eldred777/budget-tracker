module Tracker.Allocation
  ( showAllocation,
    add,
    sub,
    reallocate,
    addUnallocated,
    runAllocation,
    emptyAllocation
  )
where

import qualified Data.Map as Map
import GHC.Float (int2Float)
import Types

-- Computing allocation logic

{-
-- | Produce an allocation given a set of rules and a total to allocate from.
allocate :: MoneyTotal -> Rules -> Allocation
allocate total rules =
  let allocations = applyRules total rules
      unallocated = computeUnallocated total allocations
   in Allocation allocations unallocated
-}

applyRule :: Money -> RuleType -> Money
applyRule _ (Fixed d) = d
applyRule total (Proportion p) = (floor . (p *) . int2Float) total

applyRules :: Money -> Rules -> Allocations
applyRules total = Map.map (applyRule total)

computeUnallocated :: Money -> Allocations -> Money
computeUnallocated total = (total -) . Map.foldl (+) 0

runAllocation :: FullAllocation -> Rules -> FullAllocation
runAllocation (FullAllocation originalAllocations u) rules =
  FullAllocation (mergeAllocations originalAllocations newAllocations) unallocated
  where
    newAllocations = applyRules u rules
    unallocated = computeUnallocated u newAllocations

mergeAllocations :: Allocations -> Allocations -> Allocations
mergeAllocations = Map.unionWith (+)

add :: Allocation -> FullAllocation -> FullAllocation
add (Allocation name d) (FullAllocation allocations unallocated) =
  FullAllocation (Map.insertWith (+) name d allocations) (unallocated - d)

-- | Subtracts the amount found in `Allocation`, first checking existence of the
-- key in `FullAllocation`. `Nothing` indicates failure.
sub :: Allocation -> FullAllocation -> Maybe FullAllocation
sub (Allocation name subAmount) (FullAllocation allocations unallocated) =
  case Map.updateLookupWithKey (\_ oldAmount -> Just (oldAmount - subAmount)) name allocations of
    (Nothing, _) -> Nothing
    (Just _, newAllocations) -> Just $ FullAllocation newAllocations (unallocated + subAmount)

-- | Invariant: Money in both `Allocation`s are equal. `Command.Parser`, and the
-- requirement that Allocation be constructed there enforces this. `Nothing`
-- indicates failure.
reallocate :: Allocation -> Allocation -> FullAllocation -> Maybe FullAllocation
reallocate a1 a2 fa = add a2 <$> sub a1 fa

addUnallocated :: Money -> FullAllocation -> FullAllocation
addUnallocated x (FullAllocation a u) = FullAllocation a (x + u)

showAllocation :: FullAllocation -> String
showAllocation (FullAllocation rulesAllocations unallocated) =
  "Allocations:\n"
    ++ Map.foldlWithKey inner "" rulesAllocations
    ++ "Unallocated: "
    ++ showMoney unallocated
  where
    -- TODO: padding to longest length of name
    inner acc key val = acc ++ key ++ " -- " ++ showMoney val ++ "\n"

emptyAllocation :: FullAllocation
emptyAllocation = FullAllocation Map.empty 0 