import qualified SudokuGenerator as Generator
import qualified SudokuGame as Game
import qualified SudokuSolver as Solver
import Text.Read (readMaybe)

{- Main Menu Option Functions-}

-- | `generatePuzzleOption` is the main menu option to generate a Sudoku puzzle.
generatePuzzleOption :: IO ()
generatePuzzleOption = do
    putStrLn "Generating a Sudoku puzzle..."
    result <- Generator.generateCompletedGrid Generator.emptyGrid
    case result of
        Just completedGrid -> do
            putStrLn "Completed Grid:"
            Generator.displayGrid completedGrid
            puzzleGrid <- Generator.removeNumbers 30 completedGrid
            putStrLn "Sudoku Puzzle:"
            Generator.displayGrid puzzleGrid
        Nothing -> putStrLn "No solution found"
    mainMenu

-- | `solvePuzzleOption` is the main menu option to solve a Sudoku puzzle.
solvePuzzleOption :: IO ()
solvePuzzleOption = do
    -- Logic to solve a Sudoku puzzle
    putStrLn "Solving a Sudoku puzzle..."
    -- Implement the puzzle solving logic
    mainMenu

getDifficulty :: IO Int
getDifficulty = do
    putStrLn "Select difficulty (1-Easy, 2-Normal, 3-Heroic, 4-Legendary):"
    choice <- getLine
    case readMaybe choice of
        Just n | n `elem` [1, 2, 3, 4] -> return n
        _ -> do
            putStrLn "Invalid input, please enter a number between 1 and 4."
            getDifficulty  -- Recursive call if input is invalid

-- | `playGameOption` is the main menu option to play a Sudoku game.
playGameOption :: IO ()
playGameOption = do
    putStrLn "Starting a game of Sudoku...\n"
    -- Generate a completed grid and remove numbers to create a puzzle
    result <- Generator.generateCompletedGrid Generator.emptyGrid
    case result of
        Just completedGrid -> do
            difficulty <- getDifficulty
            let numToRemove = case difficulty of
                    1 -> 20
                    2 -> 30
                    3 -> 40
                    4 -> 50
            puzzleGrid <- Generator.removeNumbers numToRemove completedGrid
            let candidates = Game.emptyCandidates
            putStrLn "\nHow to play:"
            putStrLn "-Enter row, column, and number (e.g., 2 3 5) to input a value into a cell."
            putStrLn "-To clear cells type 'clear' followed by row and column."
            putStrLn "-To add candidates type 'addCands' followed by row, column, and a list of numbers."
            putStrLn "-To remove candidates type 'removeCand' followed by row, column, and a list of numbers."
            putStrLn "-To a list of candidates at any cell type 'getCands:' followed by the row and column."
            putStrLn "-Enter 'q' to quit.\n"
            Game.playGame puzzleGrid candidates 0
        Nothing -> putStrLn "No solution found"
    mainMenu

{- Mains-}

-- | `main` is the main function of the Sudoku program.
main :: IO ()
main = do
    putStrLn "Welcome to the Sudoku Program!"
    putStrLn "This program can generate Sudoku puzzles, solve Sudoku puzzles, and play Sudoku games."
    putStrLn "Created By Anders Evans, Thomas Hudson, and Matthew Taylor"
    mainMenu

-- | `mainMenu` is the main menu of the Sudoku program.
mainMenu :: IO ()
mainMenu = do
    putStrLn "\nPlease select an option:"
    putStrLn "1. Generate a Sudoku puzzle"
    putStrLn "2. Solve a Sudoku puzzle"
    putStrLn "3. Play Sudoku game"
    putStrLn "4. Exit"
    option <- getLine
    case option of
        "1" -> generatePuzzleOption
        "2" -> solvePuzzleOption
        "3" -> playGameOption
        "4" -> putStrLn "\nExiting demo"
        _   -> putStrLn "Invalid option. Try again." >> mainMenu

