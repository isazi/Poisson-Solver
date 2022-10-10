# Poisson Solver

This project solve Poisson's equation on a 2D grid using either:
1. A parallel openMP implementation of the Gauss-Seidel method
2. or LAPACK

The goal was to practice openMP and parallelization. This was a learning exercise and is not intended to serve as a comparison
between LAPACK and GS method.

There is a build script `scripts/build.sh`.

The binary `bin/solver` outputs the discrete grid `u_grid` to a fort file `fort.10`. You can check that both methods produce the
same answer.

## Results

For problem size:
* `ugrid[200,200]`
* `tol = 1e-11` (GS only)

For this test run I compiled the code with ifort version `2021.7.0 (oneapi)` but the code also works with GNU compiler
`gfortran`.

The test was run on CPU `Intel(R) Core(TM) i5-6400 CPU @ 2.70GHz` with 16 Gb RAM.

**Walltime (s)**

Procs | LAPACK | Gauss-Seidel
----- | ------ | ------------
1     | 823    | 25
2     | 434    | 16
4     | 265    | 11

**Maximum Memory**

LAPACK | Gauss-Seidel
------ | ------------
11.6Gb | 22Mb
