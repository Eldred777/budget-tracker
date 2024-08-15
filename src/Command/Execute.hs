-- | Dispatches to the internal API based off parsed commands.
module Command.Execute (execute) where

import Command.Parse (Command (..))
import Data.List (intercalate)
import qualified Tracker
import qualified Tracker.IO as IO
import Tracker.State (State, blankState, showState)

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
          "add [name] [$$]",
          "add [$$]",
          "sub [name] [amount]",
          "mov [name] [name] [$$]",
          "run [$$, opt]",
          "  -- Runs allocations with rules, optionally adding to unallocated count first",
          ""
        ]
  putStrLn $ intercalate "\n" ss

purer :: (Applicative f, Applicative g) => a -> f (g a)
purer = pure . pure

execute :: Command -> State -> IO (Maybe State)
execute Quit _ = return Nothing
execute ClearAll _ = printStateAndReturn blankState
execute ClearRules _ = undefined -- TODO implement execute ClearRules
execute ClearAllocation _ = undefined -- TODO implement execute ClearAllocation
execute Help state = do
  help
  purer state

-- add/delete rules/allocations
execute (NewRule rule) state =
  printStateAndReturn $ Tracker.newRule rule state
execute (DeleteRule rn) state =
  printStateAndReturn $ Tracker.deleteRule rn state
execute (Allocate a) state =
  printStateAndReturn $ Tracker.allocate a state
execute (Deallocate a) state =
  case Tracker.deallocate a state of
    Left Tracker.KeyDNE -> do
      print "Failed to find key."
      purer state
    Right newState -> do
      printStateAndReturn newState
execute (Reallocate a1 a2) state =
  case Tracker.reallocate a1 a2 state of
    Left Tracker.KeyDNE -> do
      print "Failed to find key."
      purer state
    Right s -> printStateAndReturn s
execute (RunAllocationWith x) state =
  printStateAndReturn $ Tracker.runAllocation $ Tracker.addUnallocated x state
execute RunAllocation state = printStateAndReturn $ Tracker.runAllocation state
execute (AddUnallocated m) state = printStateAndReturn $ Tracker.addUnallocated m state
-- IO
execute (Load path) _____ = do
  s <- IO.load path
  printStateAndReturn s
execute (Save path) state = do
  IO.save path state
  purer state

printStateAndReturn :: State -> IO (Maybe State)
printStateAndReturn state = do
  printState state
  purer state

printState :: State -> IO ()
printState = putStrLn . showState
