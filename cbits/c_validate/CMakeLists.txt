cmake_minimum_required(VERSION 3.20)
project(c_validate)

set (CMAKE_CXX_STANDARD 17)

include(FetchContent)
FetchContent_Declare(
        pybind11
        GIT_REPOSITORY https://github.com/pybind/pybind11
)

FetchContent_GetProperties(pybind11)
if(NOT pybind11_POPULATED)
    FetchContent_Populate(pybind11)
    add_subdirectory(${pybind11_SOURCE_DIR} ${pybind11_BINARY_DIR})
endif()

add_library(c_validate SHARED unsafe_validate.h unsafe_validate.cpp)
target_link_libraries(c_validate pybind11::embed)
