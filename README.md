# Poisson Solver

[![build](https://github.com/TomMelt/Poisson-Solver/actions/workflows/build.yml/badge.svg)](https://github.com/TomMelt/Poisson-Solver/actions/workflows/build.yml)
[![tests](https://github.com/TomMelt/Poisson-Solver/actions/workflows/tests.yml/badge.svg)](https://github.com/TomMelt/Poisson-Solver/actions/workflows/tests.yml)

This project solve Poisson's equation on a 2D grid using either:
1. A parallel openMP/openACC implementation of the Gauss-Seidel method
2. or LAPACK

The goal was to practice openMP and openACC parallelization. This was a learning exercise and is not intended to serve as a comparison
between LAPACK and GS method.

There is a build script `scripts/build.sh`.

The binary `bin/solver` outputs the discrete grid `u_grid` to a fort file `fort.10`. You can check that both methods produce the
same answer.

## Hardware Requirements

* I have only tested on NVIDIA GPUs
* For NVIDIA GPUs and openMP offloading a GPU with Compute Capability >= 7.0 is required

## Results

### openMP (CPU only)

#### Intel CPU - ifort compiler

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

#### Arm CPU - Gnu compiler

The test was run on arm CPU `Neoverse-N1` with 512 Gb RAM.

Problem size `u_grid` dims = [300,300]
number of iterations =       192018

Cores | Time (s)
------|---------
   80 |  15.4646
   64 |  14.1203
   32 |  15.4587
   16 |  21.6295
   8  |  38.4833
   4  |  70.8669
   2  | 136.7466
   1  | 270.7637

### openACC (GPU)

The test was run on nvidia GPU `NVIDIA A100` with 40 Gb RAM connected to the arm system above.

Problem size `u_grid` dims = [300,300]
number of iterations =       192018

GPU   | Time (s)
------|---------
 na   |  17.7678


## TODO

Continue to profile openACC version. Check for unneccessary memory transfers.
