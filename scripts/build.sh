set -e
rm -rf ./build-*

cmake -B ./build-gs -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=ON
(cd ./build-gs && make -j 4 && ctest -V)

cmake -B ./build-lp -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=OFF
(cd ./build-lp && make -j 4 && ctest -V)
