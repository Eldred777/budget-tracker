module Command.Parse
  ( Command (..),
    command,
    displayErr,
  )
where

import GHC.Float (float2Int)
import Text.Read (readMaybe)
import Types

type ExpectedArgCount = Int

data ParseError
  = CommandNotFound
  | InvalidArgument
  | EmptyArgument
  | InsufficientArguments ExpectedArgCount
  | NumberFormat
  | Other String
  deriving (Show, Eq)

displayErr :: ParseError -> IO ()
displayErr CommandNotFound = putStrLn "Command not found. Check `help` for valid commands."
displayErr InvalidArgument = putStrLn "Invalid argument."
displayErr EmptyArgument = putStrLn "No arguments supplied."
displayErr (InsufficientArguments expected) =
  putStrLn $ "Not enough arguments supplied, expected " ++ show expected
displayErr NumberFormat = putStrLn "Could not parse number."
displayErr (Other s) = putStrLn $ "Parsing error: " ++ s

type FileName = String

data Command
  = Help
  | Quit
  | ClearAll
  | ClearRules
  | ClearAllocation
  | NewRule Rule
  | DeleteRule RuleName
  | AddUnallocated Money
  | Allocate Allocation
  | Deallocate Allocation
  | -- | Reallocate requires that the `Allocation`s have the same quantities within them.
    Reallocate Allocation Allocation
  | RunAllocation
  | RunAllocationWith Money
  | Load FileName
  | Save FileName

-- | Parses user input into `Command`s.
-- Should be surjective!
command :: String -> Either ParseError Command
command [] = Right Quit
command s
  | w == "r" = ruleCmd args
  | w == "add" = allocate args
  | w == "sub" = deallocate args
  | w == "mov" = reallocate args
  | w == "help" = Right Help
  | w == "clear" = clear args
  | w == "clr" = clear args
  | w == "run" = run args
  | w == "exit" = Right Quit
  | w == "quit" = Right Quit
  | w == "q" = Right Quit
  | w == "load" = load args
  | w == "save" = save args
  -- TODO: clear rules, clear allocation, add unallocated
  | otherwise = Left CommandNotFound
  where
    ws = words s
    w = head ws
    args = tail ws

run :: [String] -> Either ParseError Command
run [] = Right RunAllocation
run (x : _) = case parseMoney x of
  Nothing -> Left NumberFormat
  Just xx -> Right $ RunAllocationWith xx

-- | Parses user input (prefaced by "r ") into the those `Command`s related to
-- rules, i.e. NewRule, DeleteRule
ruleCmd :: [String] -> Either ParseError Command
ruleCmd [] = Left CommandNotFound -- TODO: better error signalling
ruleCmd ws
  | head w == '-' = delRule $ tail w
  | otherwise = newRule ws
  where
    w = head ws

newRule :: [String] -> Either ParseError Command
newRule [] = Left $ InsufficientArguments 2
newRule [_] = Left $ InsufficientArguments 2
newRule (x : y : _) = case parseRuleType y of
  (Left e) -> Left e
  Right m -> Right $ NewRule (Rule x m)

delRule :: String -> Either ParseError Command
delRule [] = Left $ InsufficientArguments 1
delRule x = Right $ DeleteRule x

parseRuleType :: String -> Either ParseError RuleType
parseRuleType s =
  if last s == '%'
    then ruleTypeProp $ init s
    else ruleTypeFixed s

ruleTypeFixed, ruleTypeProp :: String -> Either ParseError RuleType
ruleTypeFixed s = case parseMoney s of
  Nothing -> Left NumberFormat
  Just x -> Right $ Fixed x
ruleTypeProp s = case readMaybe s of
  Nothing -> Left NumberFormat
  Just x -> Right $ Proportion (x / 100)

{-
>>> ruleTypeFixed "1"
>>> ruleTypeFixed "1.2"
>>> ruleTypeProp "1"
>>> ruleTypeProp "1.2"
>>> ruleTypeProp "a"
Right (Fixed 1)
Left NumberFormat
Right (Proportion 1.0)
Right (Proportion 1.2)
Left NumberFormat
-}

-- TODO: refactor thrice-repeated code

allocate, deallocate, reallocate :: [String] -> Either ParseError Command
allocate [] = Left $ InsufficientArguments 1
-- Add unallocated amount
allocate [x] = case parseMoney x of
  Nothing -> Left NumberFormat
  Just xx -> Right $ AddUnallocated xx
-- Add to an allocation
allocate (x : y : _) = case parseMoney y of
  Nothing -> Left NumberFormat
  Just yy -> Right $ Allocate (Allocation x yy)
deallocate [] = Left $ InsufficientArguments 2
deallocate [_] = Left $ InsufficientArguments 2
deallocate (x : y : _) = case parseMoney y of
  Nothing -> Left NumberFormat
  Just yy -> Right $ Deallocate (Allocation x (abs yy))
reallocate [] = Left $ InsufficientArguments 3
reallocate [_] = Left $ InsufficientArguments 3
reallocate [_, _] = Left $ InsufficientArguments 3
reallocate (x : y : z : _) = case parseMoney z of
  Nothing -> Left NumberFormat
  Just zz -> Right $ Reallocate (Allocation x zz) (Allocation y zz)

{-
>>> allocate ["aaa","123"]
>>> deallocate ["bbb","231" ]
>>> reallocate ["aaa","bbb","123123123"]
Right (Allocate "aaa" 123)
Right (Deallocate "bbb" 231)
Right (Reallocate "aaa" "bbb" 123123123)
-}

-- ? Is there a better way to do this than assume float format and convert to int? Maybe use a Decimal lib?
parseMoney :: String -> Maybe Money
parseMoney = fmap (float2Int . (100.0 *)) . readMaybe

-- IO commands

defaultFile :: String
defaultFile = "default"

save, load :: [String] -> Either ParseError Command
save [] = Right $ Save defaultFile
save (path : _) = Right $ Save path
load [] = Right $ Load defaultFile
load (path : _) = Right $ Load path

clear :: [String] -> Either ParseError Command
clear [] = Right ClearAll
clear (x : _)
  | x == "rules" = Right ClearRules
  | x == "r" = Right ClearRules
  | x == "allocation" = Right ClearAllocation
  | x == "a" = Right ClearAllocation
  | otherwise = Left InvalidArgument
