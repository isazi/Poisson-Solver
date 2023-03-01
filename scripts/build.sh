cmake -B ./build-gs -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=ON

cmake -B ./build-lp -D CMAKE_C_COMPILER=gcc -D CMAKE_CXX_COMPILER=g++ -D USEGS=OFF
