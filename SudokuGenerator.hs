{- SudokuGenerator functions as a sudoku puzzle generator and sudoku puzzle scrambler -}
module SudokuGenerator where

{- Imports -}
import Control.Monad (guard, when)
import Data.Maybe (isJust, isNothing)
import System.Random (randomRIO, newStdGen, randomRs)

{- Type Defintions -}
type Cell = Maybe Int     -- A cell is a Maybe Int
type Grid = [[Cell]]      -- A grid is a list of lists of cells

{- Sudoku Grid, Box, & Cell Generation -}

-- | `emptyGrid` generates an empty Sudoku grid.
emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 Nothing)

-- | `generateCompletedGrid` generates a completed Sudoku grid.
--   It takes an initial grid and returns a Maybe Grid representing
--   a completed Sudoku grid if one exists.
generateCompletedGrid :: Grid -> IO (Maybe Grid)
generateCompletedGrid grid
    | isFull grid = return $ Just grid
    | otherwise = do
        let (r, c) = findEmptyCell grid
        nums <- randomPermutation [1..9]
        tryNumbers r c nums grid

-- | `tryNumbers` attempts to fill a cell with numbers.
--   It takes row, column, a list of numbers to try, and the grid.
--   It returns a Maybe Grid representing the updated grid if successful,
--   or Nothing if no valid number can be placed.
tryNumbers :: Int -> Int -> [Int] -> Grid -> IO (Maybe Grid)
tryNumbers _ _ [] _ = return Nothing  -- No numbers left to try
tryNumbers r c (n:ns) grid
    | isValid n r c grid = do
        let newGrid = updateGrid r c (Just n) grid
        result <- generateCompletedGrid newGrid
        case result of
            Just g -> return $ Just g
            Nothing -> tryNumbers r c ns grid  -- Backtracks to try the next number
    | otherwise = tryNumbers r c ns grid  -- Number is not valid, try the next number

-- | `randomPermutation` generates a random permutation of a list of numbers.
randomPermutation :: [Int] -> IO [Int]
randomPermutation [] = return []
randomPermutation xs = do
    i <- randomRIO (0, length xs - 1)
    let (lead, x:rest) = splitAt i xs
    fmap (x:) $ randomPermutation (lead ++ rest)

-- | `randomList` generates a random list of numbers within a range.
randomList :: Int -> Int -> IO [Int]
randomList low high = newStdGen >>= return . randomRs (low, high)

-- | `isFull` checks if the grid is full.
isFull :: Grid -> Bool
isFull = all (all isJust)

-- | `findEmptyCell` finds the coordinates of an empty cell in the grid.
findEmptyCell :: Grid -> (Int, Int)
findEmptyCell grid = head [(r, c) | (r, row) <- zip [0..] grid, (c, cell) <- zip [0..] row, isNothing cell]

-- | `isValid` checks if a number is valid in a cell.
isValid :: Int -> Int -> Int -> Grid -> Bool
isValid n r c grid = all (validIn n) [getRow r grid, getColumn c grid, getBox r c grid]

-- | `validIn` checks if a number is valid in a list of cells.
validIn :: Int -> [Cell] -> Bool
validIn n cells = notElem (Just n) cells

-- | `getRow` gets a row from the grid.
getRow :: Int -> Grid -> [Cell]
getRow r = (!! r)

-- | `getColumn` gets a column from the grid.
getColumn :: Int -> Grid -> [Cell]
getColumn c = map (!! c)

-- | `getBox` gets a box from the grid.
getBox :: Int -> Int -> Grid -> [Cell]
getBox r c grid = [grid !! r' !! c' | r' <- boxRange r, c' <- boxRange c]

-- | `boxRange` gets the range of a box.
boxRange :: Int -> [Int]
boxRange x = let start = (x `div` 3) * 3 in [start .. start + 2]

-- | `updateGrid` updates a cell in the grid.
updateGrid :: Int -> Int -> Cell -> Grid -> Grid
updateGrid r c val grid =
    take r grid ++
    [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid

{- Helper Functions For Main Of Sudoku Generator -}

-- | `displayGrid` displays the Sudoku grid with grid lines and indices.
displayGrid :: Grid -> IO ()
displayGrid grid = do
    putStrLn "    1 2 3   4 5 6   7 8 9"
    putStrLn "  +-------+-------+-------+"
    mapM_ printRow (zip [1..] grid)
    where
        printRow (i, row) = do
            putStr (show i ++ " | ")
            putStrLn $ concatMap showCell (zip [1..] row) ++ "|"
            when (i `mod` 3 == 0) $ putStrLn "  +-------+-------+-------+"

        showCell (j, cell) = showCellValue cell ++ (if j `mod` 3 == 0 then " | " else " ")

        showCellValue Nothing = "."
        showCellValue (Just n) = show n

-- | `removeNumbers` removes 'n' numbers from the grid to create a puzzle.
removeNumbers :: Int -> Grid -> IO Grid
removeNumbers 0 grid = return grid
removeNumbers n grid = do
    row <- randomRIO (0, 8)
    col <- randomRIO (0, 8)
    let cell = grid !! row !! col
    if isNothing cell
        then removeNumbers n grid  -- Cell is already empty, try again
        else removeNumbers (n - 1) (updateGrid row col Nothing grid)

{- Main Of Sudoku Generator -}
main :: IO ()
main = do
    putStrLn "Generating a Sudoku puzzle..."
    result <- generateCompletedGrid emptyGrid
    case result of
        Just completedGrid -> do
            putStrLn "Completed Grid (for reference):"
            displayGrid completedGrid
            puzzleGrid <- removeNumbers 30 completedGrid  -- Set to removing 30 numbers as a default value
            putStrLn "Sudoku Puzzle:"
            displayGrid puzzleGrid
        Nothing -> putStrLn "No solution found"
