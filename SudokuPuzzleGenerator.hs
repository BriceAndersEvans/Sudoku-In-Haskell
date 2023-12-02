{- Sudoku Puzzle Generator -}

{- Imports -}
import Control.Monad ( guard )
import Data.Maybe ( isJust, isNothing )
import System.Random ( randomRIO )
import System.Random (newStdGen, randomRs)
import Control.Monad (guard, liftM2, when)
import Control.Monad (liftM3)
import Text.Read (readMaybe)
import System.Random (randomRIO, newStdGen, randomRs)
import Control.Monad (guard, when)

{- SUDOKU GRID GENERATION -}

{- Grid Cells -}
{- A sudoku puzzle is made of a 9x9 grid of cells -}

-- A cell can be empty (Nothing) or contain a number (0-9) (Just n)
type Cell = Maybe Int

-- The Sudoku grid is a list of rows
type Grid = [[Cell]]

-- Generates a empty Sudoku grid
emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 Nothing)

-- Generates a completed Sudoku grid
-- r is row, c is column
-- Generate a completed Sudoku grid
generateCompletedGrid :: Grid -> IO (Maybe Grid)
generateCompletedGrid grid
    | isFull grid = return $ Just grid
    | otherwise = do
        let (r, c) = findEmptyCell grid
        nums <- randomPermutation [1..9]
        tryNumbers r c nums grid

-- Try to place one of the numbers in the list into the grid
tryNumbers :: Int -> Int -> [Int] -> Grid -> IO (Maybe Grid)
tryNumbers _ _ [] _ = return Nothing  -- No number fits, backtrack
tryNumbers r c (n:ns) grid
    | isValid n r c grid = do
        let newGrid = updateGrid r c (Just n) grid
        result <- generateCompletedGrid newGrid
        case result of
            Just g -> return $ Just g
            Nothing -> tryNumbers r c ns grid  -- Backtrack and try next number
    | otherwise = tryNumbers r c ns grid  -- Current number doesn't fit, try next

-- Generate a random permutation of a list
randomPermutation :: [Int] -> IO [Int]
randomPermutation [] = return []
randomPermutation xs = do
    i <- randomRIO (0, length xs - 1)
    let (lead, x:rest) = splitAt i xs
    fmap (x:) $ randomPermutation (lead ++ rest)

-- Generate a random list of numbers within a range
randomList :: Int -> Int -> IO [Int]
randomList low high = newStdGen >>= return . randomRs (low, high)

-- Checks to see if grid is full
isFull :: Grid -> Bool
isFull = all (all isJust)

-- Finds the first empty cell in the grid
findEmptyCell :: Grid -> (Int, Int)
findEmptyCell grid = head [(r, c) | (r, row) <- zip [0..] grid, (c, cell) <- zip [0..] row, isNothing cell]

-- Checks if placing 'n' in (r, c) is valid --bugged
isValid :: Int -> Int -> Int -> Grid -> Bool
isValid n r c grid = all (validIn n) [getRow r grid, getColumn c grid, getBox r c grid]

-- Check if 'n' is not in the given list of cells
validIn :: Int -> [Cell] -> Bool
validIn n cells = notElem (Just n) cells

-- Gets a specific row
getRow :: Int -> Grid -> [Cell]
getRow r = (!! r)

-- Gets a specific column
getColumn :: Int -> Grid -> [Cell]
getColumn c = map (!! c)

-- Gets the 3x3 box containing (r, c)
getBox :: Int -> Int -> Grid -> [Cell]
getBox r c grid = [grid !! r' !! c' | r' <- boxRange r, c' <- boxRange c]

-- Gets the range of the 3x3 box containing (r, c)
boxRange :: Int -> [Int]
boxRange x = let start = (x `div` 3) * 3 in [start .. start + 2]



-- Update the grid at specificed position (r, c)
updateGrid :: Int -> Int -> Cell -> Grid -> Grid --bugged
updateGrid r c val grid = 
    take r grid ++
    [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid



{- HELPER FUNCTIONS FOR MAIN-}

-- Function to display the Sudoku grid with grid lines and indices
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

-- Remove 'n' numbers from the grid to create a puzzle
removeNumbers :: Int -> Grid -> IO Grid
removeNumbers 0 grid = return grid
removeNumbers n grid = do
    row <- randomRIO (0, 8)
    col <- randomRIO (0, 8)
    let cell = grid !! row !! col
    if isNothing cell
        then removeNumbers n grid  -- Cell is already empty, try again
        else removeNumbers (n - 1) (updateGrid row col Nothing grid)


{- MAIN -}
main :: IO ()
main = do
    putStrLn "Generating a Sudoku puzzle..."
    result <- generateCompletedGrid emptyGrid
    case result of
        Just completedGrid -> do
            putStrLn "Completed Grid (for reference):"
            displayGrid completedGrid
            puzzleGrid <- removeNumbers 30 completedGrid  -- Remove 30 numbers for example
            putStrLn "Sudoku Puzzle:"
            displayGrid puzzleGrid
            playGame puzzleGrid
        Nothing -> putStrLn "No solution found"


{- PLAYABLE GAME -}

-- Function to play the game
playGame :: Grid -> IO ()
playGame grid = do
    putStrLn "Current Sudoku Puzzle:"
    displayGrid grid
    if isFull grid && isValidGrid grid
        then putStrLn "Congratulations! You solved the puzzle!"
        else do
            putStrLn "Enter row, column, and number (e.g., 2 3 5) or 'q' to quit:"
            input <- getLine
            case input of
                "q" -> putStrLn "Exiting game."
                _   -> case parseInput input of
                          Just (r, c, n) -> if isValid n (r-1) (c-1) grid
                                                then playGame (updateGrid (r-1) (c-1) (Just n) grid)
                                                else do
                                                    putStrLn $ "Invalid move for row " ++ show r ++ ", column " ++ show c ++ ", number " ++ show n
                                                    playGame grid
                          Nothing -> putStrLn "Invalid input" >> playGame grid

{- Helper Functions for playGame-}

-- Parse user input
parseInput :: String -> Maybe (Int, Int, Int)
parseInput input = case words input of
    [rs, cs, ns] -> liftM3 (,,) (readMaybe rs) (readMaybe cs) (readMaybe ns)
    _            -> Nothing


-- Check if the entire grid is valid
isValidGrid :: Grid -> Bool
isValidGrid grid = all (\r -> all (\c -> isValidCell r c grid) [0..8]) [0..8]

-- Check if a cell is valid in the grid
isValidCell :: Int -> Int -> Grid -> Bool
isValidCell r c grid = case grid !! r !! c of
    Nothing -> True
    Just n  -> isValid n r c grid
