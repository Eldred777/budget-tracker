{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where

import qualified Command.Execute as Execute
import qualified Command.Parse as Parse
import Data.Foldable (forM_)
import System.IO (hFlush, stdout)
import Tracker.State (State, blankState)

mainLoop :: State -> IO ()
mainLoop state = do
  putStr "> "
  hFlush stdout
  userInput <- getLine
  putStrLn ""
  case Parse.command userInput of
    Right command -> do
      newState <- Execute.execute command state
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
