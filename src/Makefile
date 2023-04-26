# Distributed under MIT License
#
# Copyright (c) 2022 Tom Meltzer
#
# Please see LICENSE file for more information

# compiler
FC := $(F90)

FCFLAGS = -Wall -O3 -fopenmp -foffload=-march=gfx90a
FLFLAGS = -Wall -O3 -fopenmp

# FCFLAGS = -Wall -g -O0 -fopenmp -foffload=-march=gfx90a
# FLFLAGS = -Wall -g -O0 -fopenmp

# source files and objects
OBJ = color_mod.o config_mod.o gauss_seidel_mod.o grid_mod.o poisson_solver.o precisn_mod.o
PRE = -DUSEGS

# program name
PROGRAM = solver

all: $(PROGRAM)

.PHONY: clean

$(PROGRAM): ${OBJ}
	$(FC) $(FLFLAGS) ${PRE} -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -c $<

%.o: %.F90
	$(FC) $(FCFLAGS) ${PRE} -c $<

color_mod.o: precisn_mod.o config_mod.o
config_mod.o: precisn_mod.o
gauss_seidel_mod.o: precisn_mod.o config_mod.o color_mod.o grid_mod.o

clean:
	rm -f *.o *.mod $(PROGRAM)
