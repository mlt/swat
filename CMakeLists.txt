cmake_minimum_required(VERSION 2.8)

project(swat)
enable_language (Fortran)

option(USE_GPROF "Profile the project using gprof" OFF)
if(USE_GPROF)
  message("Adding profiling info for gprof...")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pg")
endif(USE_GPROF)

set(CMAKE_Fortran_FLAGS_COVERAGE "-fprofile-arcs -ftest-coverage")
set(CMAKE_Fortran_FLAGS_LLVM "-fplugin=dragonegg -O4")

add_subdirectory(src)
