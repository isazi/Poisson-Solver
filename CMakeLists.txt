cmake_minimum_required(VERSION 3.0)

project(Solver)

enable_language(Fortran)
enable_testing()
add_subdirectory(tests)

set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib")
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/mod")

find_package(OpenMP REQUIRED)

option(USEGS "Use Gauss-Seidel Method" ON)
if (USEGS)
    add_definitions(-DUSEGS)
else()
    find_package(BLAS REQUIRED)
    find_package(LAPACK REQUIRED)
endif()

option(USEACC "Use openACC" OFF)
option(USEGPU "Use GPU offloading" OFF)

if (USEGPU)
    add_definitions(-DUSEGPU)
endif()

if (USEGPU AND NOT CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    message(FATAL_ERROR "GPU offloading not supported for gnu compiler. Please use nvidia compiler.")
endif()

# Default assumption is openMP
set(ACCELERATOR ${OpenMP_Fortran_FLAGS})

if (USEACC)
    if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        set(ACCELERATOR "-fopenacc")
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        message(FATAL_ERROR "OpenACC is not available for the intel compiler.")
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
        set(ACCELERATOR "-acc")
    endif()
endif()

if (USEGPU)
    set(ACCTARGET "gpu")
    set(ACCELERATOR_FLAGS "${ACCELERATOR}=${ACCTARGET}")
else ()
    if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
        set(ACCELERATOR_FLAGS ${ACCELERATOR})
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
        set(ACCELERATOR_FLAGS ${ACCELERATOR})
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
        set(ACCTARGET "multicore")
        set(ACCELERATOR_FLAGS "${ACCELERATOR}=${ACCTARGET}")
    endif()
endif()

if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 ${ACCELERATOR_FLAGS} -fcheck=all -Wall -std=f2008")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 ${ACCELERATOR_FLAGS} -warn all")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NVHPC")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3 ${ACCELERATOR_FLAGS} -Wall")
endif()

set(main_sources
    "src/precisn_mod.f90"
    "src/config_mod.f90"
    "src/color_mod.f90"
    "src/grid_mod.f90"
    "src/gauss_seidel_mod.F90"
    "src/lapack_mod.F90"
    "src/poisson_solver.F90"
)

add_executable(solver ${main_sources})
target_link_libraries(solver ${LAPACK_LIBRARIES})

#add_library(precision STATIC "src/precisn_mod.f90")
#target_link_libraries(main precision)

install(TARGETS solver)
