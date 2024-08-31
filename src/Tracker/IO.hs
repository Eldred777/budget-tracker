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

type Filename = String

pathify :: String -> String
pathify filename = "./db/" ++ filename ++ ".db"

save :: Filename -> State -> IO ()
save filename (State rules allocation) = do
  let path = pathify filename
  -- overwrite file
  writeFile path $ saveRules rules
  appendFile path "\n\n"
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
  let rulesStr = head ss

  -- TODO verify this exists
  let allocationStr = head $ tail ss
  let allocationsStr = drop 1 allocationStr
  let unallocatedStr = head allocationStr

  let rules = Map.fromList $ map loadRule rulesStr
  let allocation = Map.fromList $ map loadAllocation allocationsStr
  let unallocated = read unallocatedStr :: Money
  return $ State rules (FullAllocation allocation unallocated)

loadRule :: String -> (RuleName, RuleType)
loadRule s = (name, rt)
  where
    ws = words s
    name = head ws
    qualifier = ws !! 1
    amount = ws !! 2
    rt
      | qualifier == "fixed" = Fixed (read amount)
      | qualifier == "prop" = Proportion (read amount)
      | otherwise = error "bad argument to Tracker.IO.parseRule"

loadAllocation :: String -> (RuleName, Money)
loadAllocation s = (name, money)
  where
    ws = words s
    name = head ws
    money = read $ ws !! 1

{-
>>> s <- IO.readFile "./db/default.db"
>>> s
>>> let sl = lines s
>>> sl
>>> let sls = splitOn [""] sl
>>> sls
>>> head sls
"daily prop 0.2\nfun prop 5.0e-2\ngames prop 5.0e-2\nsavings prop 0.5\nshort-term-savings prop 0.2\n\n0\ndaily 39200\nfun 9800\ngames 9800\nsavings 98000\nshort-term-savings 39200"
["daily prop 0.2","fun prop 5.0e-2","games prop 5.0e-2","savings prop 0.5","short-term-savings prop 0.2","","0","daily 39200","fun 9800","games 9800","savings 98000","short-term-savings 39200"]
[["daily prop 0.2","fun prop 5.0e-2","games prop 5.0e-2","savings prop 0.5","short-term-savings prop 0.2"],["0","daily 39200","fun 9800","games 9800","savings 98000","short-term-savings 39200"]]
["daily prop 0.2","fun prop 5.0e-2","games prop 5.0e-2","savings prop 0.5","short-term-savings prop 0.2"]
-}
