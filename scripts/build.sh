set -e
# rm -rf ./build-*

# cmake -B ./build-gs -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=ON
# (cd ./build-gs && make -j 4 && ctest -V)

# cmake -B ./build-lp -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=OFF
# (cd ./build-lp && make -j 4 && ctest -V)

module load gcc/10.3.0/nra4u3 nvhpc/22.5
module list

for compiler in "nvidia" "gnu"
do
    if [[ $compiler == "gnu" ]]; then
        CC=$(which gcc)
        FC=$(which gfortran)
    elif [[ $compiler == "nvidia" ]]; then
        CC=$(which nvc)
        FC=$(which nvfortran)
    fi
    builddir="./build-gs-$compiler-cpu"
    cmake -B $builddir \
        -D CMAKE_C_COMPILER=$CC \
        -D CMAKE_Fortran_COMPILER=$FC \
        -D USEGS=ON \
        -D USEGPU=OFF
    (cd $builddir && make VERBOSE=1)
done

compiler="nvidia"
CC=$(which nvc)
FC=$(which nvfortran)

builddir="./build-gs-$compiler-gpu"
cmake -B $builddir \
    -D CMAKE_C_COMPILER=$CC \
    -D CMAKE_Fortran_COMPILER=$FC \
    -D USEGS=ON \
    -D USEGPU=ON
(cd $builddir && make VERBOSE=1)
