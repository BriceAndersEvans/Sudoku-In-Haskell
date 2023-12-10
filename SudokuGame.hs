{- SudokuGame file has the functionality of generating a scrambled sudoku puzzle for a user to solve via terminal input-}
module SudokuGame where

{- Imports -}
import Control.Monad (guard, when, liftM2, liftM3)
import Data.Maybe (isJust, isNothing, catMaybes, fromMaybe)
import System.Random (randomRIO, newStdGen, randomRs)
import Text.Read (readMaybe)
import Data.Bool (Bool)
import Data.List (nub, delete)


{-----------------------------------}
{- Sudoku Puzzle Generator Portion -}
{-----------------------------------}

{- Type Defintions-}
type Cell = Maybe Int     -- A cell is a Maybe Int
type Grid = [[Cell]]      -- A grid is a list of lists of cells
type Candidates = [[[Maybe Int]]] -- Possible values. Each cell would have a list of corrosponding possible values

{- Sudoku Grid, Box, & Cell Generation -}

-- | `emptyGrid` generates an empty Sudoku grid.

-- | 'addCandidate' This function adds a candidate n to the cell at row r and column c
addCandidate :: Int -> Int -> Int -> Candidates -> Candidates
addCandidate r c n candidates = 
    let currentCandidates = candidates !! (r - 1) !! (c - 1) --gets an array
        newCandidates = Just n : filter (/= Just n) currentCandidates --Adds number to array if not already added
    in updateCandidates (r-1) (c-1) newCandidates candidates


getCandidates :: Int -> Int -> Candidates -> [Int]
getCandidates r c candidates = catMaybes (candidates !! (r - 1) !! (c - 1))

removeCandidate :: Int -> Int -> Int -> Candidates -> Candidates
removeCandidate r c n candidates = 
    let currentCandidates = candidates !! (r - 1) !! (c - 1)
        newCandidates = filter (/= Just n) currentCandidates
    in updateCandidates (r-1) (c-1) newCandidates candidates

updateCandidates :: Int -> Int -> [Maybe Int] -> Candidates -> Candidates
updateCandidates r c newCandidates candidates =
    take r candidates ++
    [take c (candidates !! r) ++ [newCandidates] ++ drop (c + 1) (candidates !! r)] ++
    drop (r + 1) candidates

emptyCandidates :: Candidates
emptyCandidates = replicate 9 (replicate 9 (replicate 1 Nothing))

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

-- | 'inRange'
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

-- | `getRow` gets a row from the grid.
getRow :: Int -> Grid -> [Cell]
getRow r = (!! r)

-- | `getColumn` gets a column from the grid.
getColumn :: Int -> Grid -> [Cell]
getColumn c = map (!! c)

-- | `getBox` gets a box from the grid.
getBox :: Int -> Int -> Grid -> [Cell]
getBox r c grid = [grid !! r' !! c' | r' <- boxRange r, c' <- boxRange c]
    where
        boxRange x = let start = (x `div` 3) * 3 in [start .. start + 2]

-- | `boxRange` gets the range of a box.
boxRange :: Int -> [Int]
boxRange x = let start = (x `div` 3) * 3 in [start .. start + 2]

-- | `updateGrid` updates a cell in the grid.
updateGrid :: Int -> Int -> Cell -> Grid -> Grid
updateGrid r c val grid =
    take r grid ++
    [take c (grid !! r) ++ [val] ++ drop (c + 1) (grid !! r)] ++
    drop (r + 1) grid

{- Helper Functions Sudoku Generator -}

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

{------------------------------}
{- Sudoku Puzzle Game Portion -}
{------------------------------}

{- Type Defintions-}

{- Helper Functions for playGame-}
data Command = AddCandidate Int Int Int
             | RemoveCandidate Int Int Int
             | GetCandidate Int Int
             | UpdateCell Int Int Int
             | InvalidCommand

             
-- | `parseInput` parses the user input.
parseInput :: String -> Command
parseInput input = case words input of
    ["addCandidate:", rs, cs, ns] ->
        case mapM readMaybe [rs, cs, ns] of
            Just [r, c, n] -> AddCandidate r c n
            _ -> InvalidCommand
    ["removeCandidate:", rs, cs, ns] ->
        case mapM readMaybe [rs, cs, ns] of
            Just [r, c, n] -> RemoveCandidate r c n
            _ -> InvalidCommand
    ["getCandidate:", rs, cs] ->
        case mapM readMaybe [rs, cs] of
            Just [r, c] -> GetCandidate r c
            _ -> InvalidCommand
    [rs, cs, ns] ->
        case mapM readMaybe [rs, cs, ns] of
            Just [r, c, n] -> UpdateCell r c n
            _ -> InvalidCommand
    _ -> InvalidCommand

-- | `isValidGrid` checks if the entire grid is valid
isValidGrid :: Grid -> Bool
isValidGrid grid = all isUniqueRow [0..8] && all isUniqueColumn [0..8] && all isUniqueBox [(r, c) | r <- [0, 3, 6], c <- [0, 3, 6]]
    where
        isUniqueRow r = isUnique (catMaybes $ getRow r grid)
        isUniqueColumn c = isUnique (catMaybes $ getColumn c grid)
        isUniqueBox (r, c) = isUnique (catMaybes $ getBox r c grid)

        isUnique :: [Int] -> Bool
        isUnique lst = length lst == length (nub lst)

-- | `isValidCell` checks if a cell is valid in the grid
isValidCell :: Int -> Int -> Grid -> Bool
isValidCell r c grid = case grid !! r !! c of
    Nothing -> True
    Just n  -> isValid n r c grid

{- playGame Function For Sudoku Game Puzzle-}
playGame :: Grid -> Candidates -> IO ()
playGame grid candidates = do
    putStrLn "Current Sudoku Puzzle:"
    displayGrid grid
    if isFull grid
        then if isValidGrid grid
            then putStrLn "Congratulations! You solved the puzzle!"
            else putStrLn "The puzzle is full but not solved correctly."
        else do
            input <- getLine
            case input of
                "q" -> putStrLn "Exiting game."
                _   -> case parseInput input of
                    AddCandidate r c n -> do
                        let newCandidates = addCandidate r c n candidates
                        putStrLn "+"
                        playGame grid newCandidates
                    RemoveCandidate r c n -> do
                        let newCandidates = removeCandidate r c n candidates
                        putStrLn "-"
                        playGame grid newCandidates
                    GetCandidate r c -> do
                        let cellCandidates = getCandidates r c candidates
                        putStrLn $ "Candidates for cell (" ++ show r ++ ", " ++ show c ++ "): " ++ show cellCandidates
                        playGame grid candidates
                    UpdateCell r c n -> 
                        if isValid n (r-1) (c-1) grid
                        then playGame (updateGrid (r-1) (c-1) (Just n) grid) candidates
                        else do
                            putStrLn $ "Invalid move for row " ++ show r ++ ", column " ++ show c ++ ", number " ++ show n
                            playGame grid candidates
                    InvalidCommand -> do
                        putStrLn "Invalid input"
                        playGame grid candidates

{- Main Of Sudoku Game -}
main :: IO ()
main = do
    putStrLn "Generating a Sudoku puzzle..."
    result <- generateCompletedGrid emptyGrid
    case result of
        Just completedGrid -> do
            putStrLn "Completed Grid (for reference):"
            -- displayGrid completedGrid (for reference during testing)
            puzzleGrid <- removeNumbers 30 completedGrid  -- 30 is the default value for removing numbers
            let candidates = emptyCandidates
            putStrLn "Sudoku Puzzle:"
            displayGrid puzzleGrid
            playGame puzzleGrid candidates
        Nothing -> putStrLn "No solution found"