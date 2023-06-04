cmake_minimum_required(VERSION 3.16)
project(TsRustZigDeez)

set(This TsRustZigDeez)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

FILE(GLOB Headers
  src/lexer/*.hh
  src/lexer/*.hpp
  src/parser/*.hpp
)

FILE(GLOB Sources
  src/lexer/*.cc
  src/lexer/*.cpp
  src/parser/*.cpp
)

#add_compile_options(
#  -Wall -Wpedantic -g
#)


add_compile_options(
  -Wall -Wpedantic
  -Wno-switch
  -g
  -O3
)

enable_testing()

add_subdirectory(tests)

add_library(${This}_lib STATIC ${Sources} ${Headers})
target_include_directories(${This}_lib
  PUBLIC
    src/lexer
    src/parser
)

add_executable(${This} src/main.cpp)
target_link_libraries(${This}
  ${This}_lib
)
