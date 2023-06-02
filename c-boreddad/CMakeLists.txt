cmake_minimum_required(VERSION 3.26.3)

project(monkey)

set(CMAKE_MODULE_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

set(CMAKE_C_COMPILER c89)

enable_testing()

add_compile_options(-Wall -Wextra -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align -Werror -Werror=format-security
    -fstack-protector -fstack-protector-all -fstack-protector-strong -pipe
    -Werror=implicit-function-declaration -pedantic -pedantic-errors -fstack-clash-protection -g -O2)

add_subdirectory(tests)

add_executable(${PROJECT_NAME} monkey.c lexer.c token.c repl.c)

target_include_directories(monkey PUBLIC
    "${PROJECT_BINARY_DIR}")

