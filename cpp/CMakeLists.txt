cmake_minimum_required(VERSION 3.16)
project(TsRustZigDeez)

set(This TsRustZigDeez)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_POSITION_INDEPENDENT_CODE ON)

set(Headers
  src/lexer/lexer.hh
)

set(Sources
  src/lexer/lexer.cc
)

enable_testing()

add_subdirectory(tests)

add_library(${This}_lib STATIC ${Sources} ${Headers})
target_include_directories(${This}_lib PUBLIC src)

add_executable(${This} ${Sources} ${Headers} src/main.cpp)
