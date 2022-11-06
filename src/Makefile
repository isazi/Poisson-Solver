# compiler
FC := nvfortran

FCFLAGS = -Wall -O3 -acc -Minfo=accel
FLFLAGS =

# source files and objects
OBJ = color_mod.o config_mod.o gauss_seidel_mod.o grid_mod.o poisson_solver.o precisn_mod.o
PRE = -DUSEGS

# program name
PROGRAM = solver

all: $(PROGRAM)

.PHONY: clean

$(PROGRAM): ${OBJ}
	$(FC) $(FCFLAGS) $(FLFLAGS) ${PRE} -o $@ $^

%.o: %.f90
	$(FC) $(FCFLAGS) -c $@ $<

%.o: %.F90
	$(FC) $(FCFLAGS) ${PRE} -c $@ $<

color_mod.o: precisn_mod.o config_mod.o
config_mod.o: precisn_mod.o
gauss_seidel_mod.o: precisn_mod.o config_mod.o color_mod.o grid_mod.o

clean:
	rm -f *.o *.mod $(PROGRAM)
