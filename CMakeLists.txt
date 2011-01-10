project(felt)
cmake_minimum_required(VERSION 2.8)

set(felt_VERSION_MAJOR 3)
set(felt_VERSION_MINOR 7)
set(felt_VERSION_MICRO 1)

set(X11DEF_DIR "/etc/X11/app-defaults" CACHE PATH "install directory for X11 resource files.")
configure_file(include/config.h.in include/config.h)

find_program(CPP_LOCATION cpp)
if(NOT CPP_LOCATION)
  message(FATAL_ERROR "cpp executable not found, required.")
endif()
  
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

include_directories(include)
add_subdirectory(lib)
add_subdirectory(man)
add_subdirectory(src)
