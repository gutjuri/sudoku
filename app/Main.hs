module Main where

import           Data.Sudoku
import           Data.Maybe
import           Text.Read
import           System.Environment
import           System.Random

main :: IO ()
main = do
  args <- getArgs
  if "-h" `elem` args
    then helptext
    else do
      defaultSeed <- randomIO
      let difficulty = fromMaybe 30 $ readMaybe =<< readArg args 0
          seed       = fromMaybe defaultSeed $ readMaybe =<< readArg args 1
      putStrLn
        $   fromMaybe "Please try again"
        $   pp
        <$> randomSudoku seed difficulty

readArg :: [String] -> Int -> Maybe String
readArg xs n | length xs > n = Just $ xs !! n
             | otherwise     = Nothing

helptext :: IO ()
helptext = do
  putStrLn "Sudoku Generator by Juri Dispan"
  putStrLn "Usage: sudoku-gen [DIFFICULTY [SEED]]"
