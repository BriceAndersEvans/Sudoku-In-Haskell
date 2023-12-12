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
    | isFull grid = return $ Just grid -- If the input grid is already full, return it as a completed Sudoku grid.
    | otherwise = do -- If the input grid is not full, proceed with generating the missing elements.
        -- Find the coordinates (r, c) of the first empty cell in the grid.
        let (r, c) = findEmptyCell grid  
        -- Generate a random permutation of numbers from 1 to 9.
        nums <- randomPermutation [1..9]
        -- Try filling in the empty cell (r, c) with the generated numbers.
        tryNumbers r c nums grid

-- | `tryNumbers` attempts to fill a cell with numbers.
--   It takes row, column, a list of numbers to try, and the grid.
--   It returns a Maybe Grid representing the updated grid if successful,
--   or Nothing if no valid number can be placed.
tryNumbers :: Int -> Int -> [Int] -> Grid -> IO (Maybe Grid)
tryNumbers _ _ [] _ = return Nothing  -- No numbers left to try
-- Try each number in the list one by one
tryNumbers r c (n:ns) grid
    | isValid n r c grid = do
        -- If the number 'n' is valid at the (r, c) position, update the grid
        let newGrid = updateGrid r c (Just n) grid
        -- Generate a completed grid from the updated grid
        result <- generateCompletedGrid newGrid
        -- Check if the generated grid is valid
        case result of
            Just g -> return $ Just g  -- If valid, return the completed grid
            Nothing -> tryNumbers r c ns grid  -- If not valid, backtrack to try the next number
    | otherwise = tryNumbers r c ns grid  -- If 'n' is not valid, try the next number in the list


-- | `randomPermutation` generates a random permutation of a list of integers.
randomPermutation :: [Int] -> IO [Int]
-- If the input list is empty, return an empty list (base case).
randomPermutation [] = return []
-- If the input list is not empty, proceed with generating the permutation.
randomPermutation xs = do
    -- Generate a random index 'i' within the bounds of the list.
    i <- randomRIO (0, length xs - 1)
    -- Split the list at the random index 'i'.
    let (lead, x:rest) = splitAt i xs
    -- Recursively call 'randomPermutation' on the remaining list elements
    -- and prepend 'x' to the result.
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
-- Update the cell at row 'r' and column 'c' with the given 'val' in the 'grid'.
updateGrid r c val grid =
    -- Take all rows up to row 'r' as they are in the original grid.
    take r grid ++
    -- Update the row at index 'r' with the new cell value 'val'.
    [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
    -- Take all rows after row 'r' as they are in the original grid.
    drop (r + 1) grid

{- Helper Functions For Main Of Sudoku Generator -}

-- | `displayGrid` displays the Sudoku grid with grid lines and indices.
displayGrid :: Grid -> IO ()

-- Display the Sudoku grid with proper formatting.
displayGrid grid = do
    putStrLn "    1 2 3   4 5 6   7 8 9"  -- Print column indices
    putStrLn "  +-------+-------+-------+"  -- Print top border
    mapM_ printRow (zip [1..] grid)  -- Print each row with proper formatting
    where
        printRow (i, row) = do
            putStr (show i ++ " | ")  -- Print row index and separator
            putStrLn $ concatMap showCell (zip [1..] row) ++ "|"  -- Print row content
            when (i `mod` 3 == 0) $ putStrLn "  +-------+-------+-------+"  -- Print separator after every 3 rows

        showCell (j, cell) = showCellValue cell ++ (if j `mod` 3 == 0 then " | " else " ")  -- Format cell content

        showCellValue Nothing = "."  -- Display empty cell as '.'
        showCellValue (Just n) = show n  -- Display filled cell as the number 'n'


-- | `removeNumbers` removes 'n' numbers from the grid to create a puzzle.
removeNumbers :: Int -> Grid -> IO Grid
-- If 'n' becomes 0, return the resulting grid.
removeNumbers 0 grid = return grid
-- If 'n' is not 0, proceed with removing 'n' numbers.
removeNumbers n grid = do
    -- Generate random row and column indices.
    row <- randomRIO (0, 8)
    col <- randomRIO (0, 8)
    -- Get the value of the cell at the randomly selected position.
    let cell = grid !! row !! col
    -- Check if the cell is already empty (Nothing).
    if isNothing cell
        then removeNumbers n grid  -- Cell is already empty, try again
        else removeNumbers (n - 1) (updateGrid row col Nothing grid)
        -- If the cell is not empty, set it to Nothing (empty) and continue with removing 'n - 1' numbers.

{- Main Of Sudoku Generator -}
-- Define the main function for the Sudoku puzzle generation.
main :: IO ()
-- Start the main function.
main = do
    putStrLn "Generating a Sudoku puzzle..."  -- Display a message to indicate puzzle generation is starting
    -- Generate a completed Sudoku grid.
    result <- generateCompletedGrid emptyGrid
    -- Check if a completed grid was successfully generated.
    case result of
        Just completedGrid -> do
            putStrLn "Completed Grid (for reference):"
            displayGrid completedGrid  -- Display the completed grid for reference
            -- Remove numbers to create the Sudoku puzzle (default: remove 30 numbers).
            puzzleGrid <- removeNumbers 30 completedGrid
            putStrLn "Sudoku Puzzle:"
            displayGrid puzzleGrid  -- Display the Sudoku puzzle grid
        Nothing -> putStrLn "No solution found"  -- Display a message if no solution was found

