import qualified SudokuGenerator as Generator
import qualified SudokuGame as Game
import qualified SudokuSolver as Solver

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
        "4" -> putStrLn "Exiting the program."
        _   -> putStrLn "Invalid option, please try again." >> mainMenu

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

-- | `playGameOption` is the main menu option to play a Sudoku game.
playGameOption :: IO ()
playGameOption = do
    putStrLn "Starting a game of Sudoku..."
    -- Generate a completed grid and remove numbers to create a puzzle
    result <- Generator.generateCompletedGrid Generator.emptyGrid
    case result of
        Just completedGrid -> do
            puzzleGrid <- Generator.removeNumbers 30 completedGrid
            putStrLn "Sudoku Puzzle:"
            Generator.displayGrid puzzleGrid
            Game.playGame puzzleGrid
        Nothing -> putStrLn "No solution found"
    mainMenu