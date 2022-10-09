cmake_minimum_required(VERSION 3.0)

project(GaussSeidel)

enable_language(Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fopenmp -fcheck=all -Wall -std=f2008")

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod")

find_package(OpenMP REQUIRED)
#find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)

option(USEGS "Use Gauss-Seidel Method" ON)
if (USEGS)
  add_definitions(-DUSEGS)
endif()

set(main_sources
    "src/color_mod.f90"
    "src/config_mod.f90"
    "src/grid_mod.f90"
    "src/precisn_mod.f90"
    "src/gs-solve.F90"
)

add_executable(gs-solve ${main_sources})
target_link_libraries(gs-solve ${LAPACK_LIBRARIES})

#add_library(precision STATIC "src/precisn_mod.f90")
#target_link_libraries(main precision)

