# Poisson Solver

This project solve Poisson's equation on a 2D grid using either:
1. A parallel openMP implementation of the Gauss-Seidel method
2. or LAPACK

The goal was to practice openMP and parallelization. This was a learning exercise and is not intended to serve as a comparison
between LAPACK and GS method.

## Results

For problem size:
* `ugrid[200,200]`
* `tol = 1e-08`

Compiled with ifort version `2021.7.0 (oneapi)` running on `Intel(R) Core(TM) i5-6400 CPU @ 2.70GHz`

**Walltime (s)**

Procs | LAPACK | Gauss-Seidel
----- | ------ | ------------
1     | 823    | 17
2     | -      | 11
4     | 265    | 7

**Maximum Memory**

LAPACK | Gauss-Seidel
------ | ------------
11.6Gb | 22Mb
