cmake_minimum_required(VERSION 3.14)
project(xylo)

include(FetchContent)


# LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM: ${LLVM_DIR} (found version \"${LLVM_PACKAGE_VERSION}\")")

include_directories(SYSTEM ${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})
llvm_map_components_to_libnames(LLVM_LIBS support core irreader native orcjit)


# Boehm GC
find_path(BOEHMGC_INCLUDE_DIR NAMES gc/gc.h)
find_library(BOEHMGC_LIBRARY NAMES gc boehm-gc bdwgc)
if(BOEHMGC_INCLUDE_DIR AND BOEHMGC_LIBRARY)
  message(STATUS "Found Boehm GC: ${BOEHMGC_LIBRARY}")
else()
  message(FATAL_ERROR "Boehm GC (libgc) not found")
endif()

add_library(BoehmGC INTERFACE)
target_include_directories(BoehmGC INTERFACE "${BOEHMGC_INCLUDE_DIR}")
target_link_libraries(BoehmGC INTERFACE "${BOEHMGC_LIBRARY}")


# GoogleTest
set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
set(BUILD_GMOCK OFF CACHE BOOL "" FORCE)
FetchContent_Declare(
  googletest
  URL https://github.com/google/googletest/archive/refs/tags/v1.17.0.zip
  DOWNLOAD_EXTRACT_TIMESTAMP ON
)
FetchContent_MakeAvailable(googletest)


set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

add_compile_options(-std=c++20 -Werror -Wall -Wextra -Wpedantic -Wno-unused-parameter)
include_directories("${PROJECT_SOURCE_DIR}/include")


if (CMAKE_BUILD_TYPE STREQUAL "Debug")
  if (CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    add_compile_options(-fprofile-instr-generate -fcoverage-mapping)
    add_link_options(-fprofile-instr-generate)
  endif()
  if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    add_compile_options(--coverage)
    add_link_options(--coverage)
  endif()
endif()


enable_testing()

add_subdirectory(src)
add_subdirectory(test)
