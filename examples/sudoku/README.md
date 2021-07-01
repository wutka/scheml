# A Sudoku Solver in Scheml
The sudoku.scm file implements a Sudoku solver in Scheml. The comments in
the file explain how it works. This directory include two files from
Peter Norvig's page on Sudoku solving (https://norvig.com/sudoku.html).
To run the solver against one of these files:
```shell
../../scheml sudoku.scm
(solve-sudoku-file "easy50.txt" "easy50-results.txt")
```

The top95.txt file can take about 20 minutes to run.
