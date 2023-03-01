# Poisson Solver

![build status](https://github.com/TomMelt/openMP-Poisson/actions/workflows/cmake.yaml/badge.svg)

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

**TESTS**

The test was run on arm CPU `Neoverse-N1` with 512 Gb RAM.

Problem size `u_grid` dims = [300,300]
number of iterations =       192018

Cores | Time (s)
------|---------
   80 |  36.0912
   64 |  31.6890
   32 |  35.1820
   16 |  43.3966
   8  |  60.4974
   4  |  94.1945
   2  | 159.6329
   1  | 218.5429

The test was run on nvidia GPU `NVIDIA A100` with 40 Gb RAM connected to the arm system above.

Problem size `u_grid` dims = [300,300]
number of iterations =       192018

GPU   | Time (s)
------|---------
 na   |  18.1043
