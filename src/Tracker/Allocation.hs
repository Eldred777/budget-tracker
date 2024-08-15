module Tracker.Allocation
  ( showAllocation,
    runRules,
    merge,
    add,
    sub,
    reallocate,
    addUnallocated,
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

-- | Runs rules, producing an allocation
runRules :: Money -> Rules -> FullAllocation
runRules total rules = FullAllocation allocated unallocated
  where
    allocated = applyRules total rules
    unallocated = computeUnallocated total allocated

merge :: FullAllocation -> FullAllocation -> FullAllocation
merge (FullAllocation a1 u1) (FullAllocation a2 u2) =
  FullAllocation (Map.unionWith (+) a1 a2) (u1 + u2)

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
