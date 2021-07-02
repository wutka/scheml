# A Sudoku Solver in Scheml
The sudoku.scm file implements a Sudoku solver in Scheml. The comments in
the file explain how it works. This directory include two files from
Peter Norvig's page on Sudoku solving (https://norvig.com/sudoku.html).
To run the solver against one of these files:
```shell
../../scheml sudoku.scm
(solve-sudoku-file "easy50.txt" "easy50-results.txt")
```

If you run scheml from a terminal window that supports ANSI escape sequences,
you can run `(define *sudoku-display* #t)` to display the sudoku as it tries
to solve it.

The top95.txt file can take about 20 minutes to run.
