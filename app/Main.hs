{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Main where

import qualified Command.Execute as Execute
import qualified Command.Parse as Parse
import Data.Foldable (forM_)
import Tracker.State ( State, blankState, showState )
import System.IO (hFlush, stdout)

mainLoop :: State -> IO ()
mainLoop state = do
  putStrLn $ showState state
  putStrLn ""
  putStrLn "What do you want me to do? (empty to quit)"
  putStr "> "
  hFlush stdout
  s <- getLine
  putStrLn ""
  case Parse.command s of
    Right command -> do
      newState <- Execute.execute command state
      -- \/ execute only if newState is not Nothing
      forM_ newState mainLoop
    Left parseErr -> do
      Parse.displayErr parseErr
      mainLoop state

main :: IO ()
main = do
  putStrLn "Hello, world!"
  -- TODO loading from database on init?
  let emptyState = blankState
  mainLoop emptyState
  putStrLn "Bye bye!"
