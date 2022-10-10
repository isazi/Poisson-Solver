cmake -B ./build-gs -D CMAKE_C_COMPILER=icc -D CMAKE_CXX_COMPILER=icpc -D USEGS=ON

cmake -B ./build-lp -D CMAKE_C_COMPILER=icc -D CMAKE_CXX_COMPILER=icpc -D USEGS=OFF
