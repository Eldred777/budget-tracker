-- | Dispatches to the internal API based off parsed commands.
module Command.Execute (execute) where

import Command.Parse (Command (..))
import Data.List (intercalate)
import qualified Tracker
import qualified Tracker.IO as IO
import Tracker.State (State, blankState)

help :: IO ()
help = do
  -- TODO: finish writing help command
  let ss =
        [ "Help menu",
          "Names may not contain spaces.",
          "",
          "clear, clr",
          "  -- Resets state of rules and allocations to null.",
          "q, quit, exit",
          "  -- Quit.",
          -- "clear rules : Resets state of ",
          -- "clear allocations : Resets state of ",
          "",
          "r [name] [$$]",
          "r [name] [number]%",
          "  -- Add a new rule, overwriting if a rule with the same name already exists.",
          "     Supports fixed and percentage allocations from the unallocated pool.",
          "r -[name]",
          "  -- Delete an existing rule.",
          "",
          "add [name] [amount]",
          "rem [name] [amount]",
          "mov [name] [name] [amount]",
          "run",
          "  -- Runs ",
          ""
        ]
  putStrLn $ intercalate "\n" ss

purer :: (Applicative f, Applicative g) => a -> f (g a)
purer = pure . pure

execute :: Command -> State -> IO (Maybe State)
execute Quit _ = return Nothing
execute ClearAll _ = purer blankState
execute ClearRules _ = undefined -- TODO implement execute ClearRules
execute ClearAllocation _ = undefined -- TODO implement execute ClearAllocation
execute Help state = do
  help
  purer state

-- add/delete rules/allocations
execute (NewRule rule) state = purer $ Tracker.newRule rule state
execute (DeleteRule rn) state = purer $ Tracker.deleteRule rn state
execute (Allocate a) state = purer $ Tracker.allocate a state
execute (Deallocate a) state = case Tracker.deallocate a state of
  Left Tracker.KeyDNE -> do
    print "Failed to find key."
    purer state
  Right newState -> purer newState
execute (Reallocate a1 a2) state = case Tracker.reallocate a1 a2 state of
  Left Tracker.KeyDNE -> undefined -- TODO proper implementation
  Right s -> purer s
execute RunAllocation state = purer $ Tracker.runAllocation state
execute (AddUnallocated m) state = purer $ Tracker.addUnallocated m state

-- IO
execute (Load path) _____ = do
  s <- IO.load path
  purer s
execute (Save path) state = do
  IO.save path state
  purer state
