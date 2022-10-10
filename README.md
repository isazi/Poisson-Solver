# Poisson Solver

This project solve Poisson's equation on a 2D grid using either:
1. A parallel openMP implementation of the Gauss-Seidel method
2. or LAPACK

The goal was to practice openMP and parallelization.

## Results

For problem size:
* `ugrid[200,200]`
* `tol = 1e-08`

### Walltime (s)

Procs | LAPACK | Gauss-Seidel
----- | ------ | ------------
1     | 823    | 17
2     | -      | 11
4     | 265    | 7

### Walltime (s)

LAPACK | Gauss-Seidel
------ | ------------
11.6Gb | 22Mb
