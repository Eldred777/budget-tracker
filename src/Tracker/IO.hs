module Tracker.IO
  ( save,
    load,
  )
where

import Data.List (intercalate)
import Data.List.Split
import qualified Data.Map as Map
import qualified System.IO as IO
import Tracker.State
import Types

type Filename = String -- ?

pathify :: String -> String
pathify filename = "./db/" ++ filename ++ ".db"

save :: Filename -> State -> IO ()
save filename (State rules allocation) = do
  let path = pathify filename
  -- overwrite file
  writeFile path $ saveRules rules
  appendFile path "\n"
  appendFile path $ saveAllocation allocation

-- | Transform rules into a save-able format
saveRules :: Rules -> String
saveRules = intercalate "\n" . Map.foldrWithKey inner []
  where
    inner :: RuleName -> RuleType -> [String] -> [String]
    inner name (Fixed x) l = (name ++ " fixed " ++ show x) : l
    inner name (Proportion x) l = (name ++ " prop " ++ show x) : l

-- | Transform allocation into a save-able format
saveAllocation :: FullAllocation -> String
saveAllocation (FullAllocation allocations unallocated) =
  intercalate "\n" $ show unallocated : Map.foldrWithKey inner [] allocations
  where
    inner :: RuleName -> Money -> [String] -> [String]
    inner name amount l = (name ++ " " ++ show amount) : l

-- TODO Either to catch errors. Unsafe atm.
load :: Filename -> IO State
load filename = do
  let path = pathify filename
  s <- IO.readFile path
  let ss = splitOn [""] $ lines s
  let rules = head ss
  -- TODO verify this exists
  let allocation_s = head $ tail ss
  let allocations_s = drop 1 allocation_s
  let unallocated_s = head allocation_s
  let state = Map.fromList $ map parseRule rules
  let allocation = Map.fromList $ map parseAllocation allocations_s
  let unallocated = read unallocated_s :: Money
  return $ State state (FullAllocation allocation unallocated)

parseRule :: String -> (RuleName, RuleType)
parseRule s = (name, rt)
  where
    ws = words s
    name = head ws
    qualifier = ws !! 1
    amount = ws !! 2
    rt
      | qualifier == "fixed" = Fixed (read amount)
      | qualifier == "prop" = Proportion (read amount)
      | otherwise = error "bad argument to Tracker.IO.parseRule"

parseAllocation :: String -> (RuleName, Money)
parseAllocation s = (name, money)
  where
    ws = words s
    name = head ws
    money = read $ ws !! 1
