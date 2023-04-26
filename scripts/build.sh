# set -e
rm -rf ./build-*

# cmake -B ./build-gs -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=ON
# (cd ./build-gs && make -j 4 && ctest -V)

# cmake -B ./build-lp -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=OFF
# (cd ./build-lp && make -j 4 && ctest -V)

module load gcc/10.3.0/nra4u3 nvhpc/22.5

export OMP_NUM_THREADS=16
export ACC_NUM_CORES=16

for openacc in "ON" "OFF"
do
    for compiler in "nvidia" "gnu"
    do
        if [[ $compiler == "gnu" ]]; then
            CC=$(which gcc)
            FC=$(which gfortran)
        elif [[ $compiler == "nvidia" ]]; then
            CC=$(which nvc)
            FC=$(which nvfortran)
        fi
        if [[ $openacc == "ON" ]]; then
            acc="acc"
        else
            acc="omp"
        fi
        builddir="./build-gs-$compiler-cpu-$acc"
        cmake -B $builddir \
            -D CMAKE_C_COMPILER=$CC \
            -D CMAKE_Fortran_COMPILER=$FC \
            -D USEGS=ON \
            -D USEACC=$openacc \
            -D USEGPU=OFF
        (cd $builddir && make VERBOSE=1)
        echo "======================================================"
        echo "Running test in $builddir ..."
        (cd $builddir && ctest)
        # (cd $builddir && ./bin/solver 200 10000000)
    done
done

compiler="nvidia"
CC=$(which nvc)
FC=$(which nvfortran)

for openacc in "ON" "OFF"
do
    if [[ $openacc == "ON" ]]; then
        acc="acc"
    else
        acc="omp"
    fi
    builddir="./build-gs-$compiler-gpu-$acc"
    cmake -B $builddir \
        -D CMAKE_C_COMPILER=$CC \
        -D CMAKE_Fortran_COMPILER=$FC \
        -D USEGS=ON \
        -D USEACC=$openacc \
        -D USEGPU=ON
    (cd $builddir && make VERBOSE=1)
    echo "======================================================"
    echo "Running test in $builddir ..."
    (cd $builddir && ctest)
    # (cd $builddir && ./bin/solver 200 10000000)
done
