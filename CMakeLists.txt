project(felt)
cmake_minimum_required(VERSION 2.8)
set(felt_VERSION_MAJOR 3)
set(felt_VERSION_MINOR 1)
set(CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/Modules)

FIND_PACKAGE(FLEX REQUIRED)
FIND_PACKAGE(BISON REQUIRED)

include_directories(include)
add_subdirectory(lib)
add_subdirectory(src)
