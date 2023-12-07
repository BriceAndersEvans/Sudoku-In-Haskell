# Sudoku In Haskell

<Overview>

The Sudoku In Haskell is a Haskell-based program designed for Sudoku enthusiasts and programmers alike. This comprehensive tool offers functionalities to generate Sudoku puzzles, solve given puzzles, and play Sudoku interactively. It showcases the power of Haskell in handling complex logic and user interaction in a text-based environment.

Features
    Puzzle Generation: Automatically generate Sudoku puzzles with varying difficulty levels.
    Interactive Gameplay: Play Sudoku in a user-friendly, text-based interface.
    Puzzle Solver: Input your own Sudoku puzzles and get them solved by the program.
    Modular Design: The application is divided into distinct modules for ease of understanding and modification.

<Getting Started>

Prerequisites: 
- Ensure you have the Glasgow Haskell Compiler (GHC) installed on your system. GHC can be downloaded from The Haskell Tool Stack.
- Ensure uou have the Git Version Control (Git) installed on your system. Git can be installed from the 

Installation: Clone the repository:  git clone https://github.com/your-username/sudoku-haskell.git

Navigate to the project directory: cd sudoku-haskell

<Running the Program>

Compile the program using GHC: ghc --make Main.hs

Execute the compiled program: ./Main

<Usage>

Upon launching the program, you will be presented with a main menu offering various options:

    Generate a Sudoku Puzzle: Automatically generate a new puzzle.
    Solve a Sudoku Puzzle: Enter a puzzle and have the program solve it.
    Play Sudoku Game: Interactively play Sudoku.
    Exit: Exit the program.

Navigate through these options by entering the corresponding number.
Modules

    SudokuGenerator: Handles the generation of Sudoku puzzles.
    SudokuSolver: Solves user inputed Sudoku puzzles.
    SudokuGame: Manages the interactive gameplay.
    SudokuMain: The entry point of the program, integrating functionalities from other modules.

