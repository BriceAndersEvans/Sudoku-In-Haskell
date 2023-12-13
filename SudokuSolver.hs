{- SudokuGenerator file has the functionality of taking a partially completed sudoku puzzle as terminal input and outputting a solution to the puzzle-}
module SudokuSolver where

{- Imports -}
import Control.Monad (guard, when, liftM2, liftM3)
import Data.Maybe (isJust, isNothing)
import System.Random (randomRIO, newStdGen, randomRs)
import Text.Read (readMaybe)

type Cell = Maybe Int     -- A cell is a Maybe Int
type Grid = [[Cell]]      -- A grid is a list of lists of cells

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

-- | `isFull` checks if the grid is full.
isFull :: Grid -> Bool
isFull = all (all isJust)

-- | `findEmptyCell` finds the coordinates of an empty cell in the grid.
findEmptyCell :: Grid -> (Int, Int)
findEmptyCell grid = head [(r, c) | (r, row) <- zip [0..] grid, (c, cell) <- zip [0..] row, isNothing cell]

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

-- | 'inRange' checks if 
inRange :: Int -> Int -> Bool
inRange r c 
    | ( r > -1 && r < 9 ) && ( c > -1 && c < 9) = True
    | otherwise = False;

-- | `isValid` checks if a number is valid in a cell.
isValid :: Int -> Int -> Int -> Grid -> Bool
isValid n r c grid = (inRange r c) && (all (validIn n) [getRow r grid, getColumn c grid, getBox r c grid])
-- | `validIn` checks if a number is valid in a list of cells. Also checks if n is in range
validIn :: Int -> [Cell] -> Bool
validIn n cells = ( n > 0 && n < 10 ) && (notElem (Just n) cells)

-- | `displayGrid` displays the Sudoku grid with grid lines and indices.
displayGrid :: Grid -> IO ()

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

-- | 'clearCell' removes a Maybe Int from a cell on the sudoku board and replaces it with Nothing.
clearCell :: Int -> Int -> Grid -> Grid
clearCell r c grid =
    take (r - 1) grid ++  -- Takes all the rows before the target row
    [take (c - 1) (grid !! (r - 1)) ++ [Nothing] ++ drop c (grid !! (r - 1))] ++  -- Constructs the target row with the cleared cell
    drop r grid  -- Takes all the rows after the target row


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

-- | `parseInput` parses the user input.
parseInput :: String -> Command
parseInput input = case words input of
    ["clear", rs, cs] ->
        case mapM readMaybe [rs, cs] of
            Just [r, c] -> Clear r c
            _ -> InvalidCommand
    [rs, cs, ns] ->
        case mapM readMaybe [rs, cs, ns] of
            Just [r, c, n] -> UpdateCell r c n
            _ -> InvalidCommand
    _ -> InvalidCommand


{------------------------------}
{- Sudoku Puzzle Solver Portion -}
{------------------------------}

{-SolveGrid without having a prevously defined grid-}
solveMyGridNew:: IO()
solveMyGridNew = solveMyGrid emptyGrid

{- Helper Functions for solveMyGrid-}
data Command = UpdateCell Int Int Int
             | Clear Int Int
             | InvalidCommand


{-Solve a grid from user input-}
solveMyGrid :: Grid -> IO()
solveMyGrid g = do
    mg <- generateCompletedGrid g
    -- eventually add check to see if mg is actually solved
    putStrLn "To add a cell: 'row(1-9) column(1-9) number(1-9)'"
    putStrLn "To clear a cell: 'clear, row(1-9), column(1-9)'"
    putStrLn "To display grid: 'p'" 
    putStrLn "To solve: 's'"
    input <- getLine
    case input of 
        "s" -> case mg of
            Just completedGrid -> do 
                displayGrid completedGrid
        "p" -> do
            displayGrid g
            solveMyGrid g
        _ -> case parseInput input of
                        UpdateCell r c n -> 
                            if isValid n (r-1) (c-1) g
                            then solveMyGrid (updateGrid (r-1) (c-1) (Just n) g)
                            else do
                                putStrLn $ if inRange (r-1) (c-1) 
                                        then "Invalid move for row " ++ show r ++ ", column " ++ show c  ++ ", number " ++ show n
                                        else "Cell out of bounds for row" ++ show r ++ ", column " ++ show c ++ ", number " ++ show n
                                solveMyGrid g
                        Clear r c -> 
                            if inRange (r-1) (c-1)
                            then solveMyGrid (clearCell r c g)
                            else do
                                putStrLn $ "Cell out of bounds"
                                solveMyGrid g
                        InvalidCommand -> do
                            putStrLn "Invalid input" >> solveMyGrid g

