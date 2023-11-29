{- Sudoku Puzzle Generator -}

{- Imports -}
import Control.Monad ( guard )
import Data.Maybe ( isJust, isNothing )
import System.Random ( randomRIO )
import System.Random (newStdGen, randomRs)
import Control.Monad (guard, liftM2)


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

-- Checks if placing 'n' in (r, c) is valid
isValid :: Int -> Int -> Int -> Grid -> Bool
isValid n r c grid = all (validIn n) [getRow r grid, getColumn c grid, getBox r c grid]

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

-- Check if 'n' is not in the given list of cells
validIn :: Int -> [Cell] -> Bool
validIn n cells = notElem (Just n) cells

-- Update the grid at specificed position (r, c)
updateGrid :: Int -> Int -> Cell -> Grid -> Grid
updateGrid r c val grid = 
    take r grid ++
    [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid

{- HELPER FUNCTIONS FOR MAIN-}
-- Function to display the grid
printGrid :: Grid -> IO ()
printGrid grid = mapM_ printRow grid
  where
    printRow :: [Cell] -> IO ()
    printRow row = putStrLn $ unwords $ map showCell row

    showCell :: Cell -> String
    showCell Nothing  = "."
    showCell (Just n) = show n


{- MAIN -}
main :: IO ()
main = do
    putStrLn "Generating a completed Sudoku grid..."
    result <- generateCompletedGrid emptyGrid
    case result of
        Just grid -> do
            putStrLn "Completed Grid:"
            printGrid grid
            -- Add logic to remove numbers and create a puzzle
        Nothing -> putStrLn "No solution found"
