cmake_minimum_required(VERSION 3.16)
project(TsRustZigDeez CXX)


if(CMAKE_CXX_COMPILER_ID  STREQUAL "GNU")
  if (CMAKE_CXX_COMPILER_VERSION VERSION_LESS 12.0)
    message(FATAL_ERROR "GCC version must be at least 12.0!")
  endif()
elseif (CMAKE_CXX_COMPILER_ID  STREQUAL "Clang")
  message(FATAL_ERROR "Clang not supported!")
else()
  message(WARNING "You are using an unsupported compiler! ${CMAKE_CXX_COMPILER_ID} Compilation has only been tested with GCC >= 12.")
endif()



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


install(
  TARGETS ${This}
  RUNTIME DESTINATION bin
)

install(
  FILES prelude
  DESTINATION .
)

