find_package(check REQUIRED)

include(CheckCSourceCompiles)
include(CheckCSourceRuns)
include(CheckFunctionExists)
include(CheckIncludeFile)
include(CheckIncludeFiles)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckTypeSize)

add_executable(check_lexer check_lexer.c ../lexer.c ../token.c)
target_link_libraries(check_lexer PUBLIC check pthread)

add_test(NAME check_lexer COMMAND check_lexer WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Testing)
set_tests_properties(check_lexer PROPERTIES TIMEOUT 30)

add_executable(check_parser check_parser.c ../parser.c ../lexer.c ../token.c)
target_link_libraries(check_parser PUBLIC check pthread)

add_test(NAME check_parser COMMAND check_parser WORKING_DIRECTORY ${CMAKE_BINARY_DIR}/Testing)
set_tests_properties(check_parser PROPERTIES TIMEOUT 30)
