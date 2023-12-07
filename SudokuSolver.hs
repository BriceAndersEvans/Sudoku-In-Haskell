{- SudokuGenerator file has the functionality of taking a partially completed sudoku puzzle as terminal input and outputting a solution to the puzzle-}
module SudokuSolver where

{- Imports -}
import Control.Monad (guard, when, liftM2, liftM3)
import Data.Maybe (isJust, isNothing)
import System.Random (randomRIO, newStdGen, randomRs)
import Text.Read (readMaybe)

