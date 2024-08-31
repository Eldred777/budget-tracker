module Tracker.Rules
  ( insert,
    delete,
    showRules,
    emptyRules,
  )
where

import qualified Data.Map as Map
import Types

insert :: Rule -> Rules -> Rules
insert (Rule key val) = Map.insert key val

delete :: RuleName -> Rules -> Rules
delete = Map.delete

showRules :: Rules -> String
showRules = ("Rules:\n" ++) . Map.foldlWithKey inner ""
  where
    inner a k b = a ++ k ++ " " ++ show b ++ "\n"

emptyRules :: Rules
emptyRules = Map.empty