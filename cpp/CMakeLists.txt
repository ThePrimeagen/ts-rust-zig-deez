cmake_minimum_required(VERSION 3.20)

project(
    monkey
    VERSION 1.0.0
    LANGUAGES CXX
)

add_executable(TESTS tests.cc)
add_executable(${PROJECT_NAME} repl.cc)

set_property(
    TARGET ${PROJECT_NAME}
    PROPERTY CXX_STANDARD 20
)
set_property(
    TARGET ${PROJECT_NAME}
    PROPERTY CXX_STANDARD_REQUIRED true
)

set_property(
    TARGET TESTS
    PROPERTY CXX_STANDARD 20
)
set_property(
    TARGET TESTS
    PROPERTY CXX_STANDARD_REQUIRED true
)
if (MSVC)
    target_include_directories(TESTS
        PRIVATE ${CMAKE_SOURCE_DIR}
        "C:/Program Files (x86)/Visual Leak Detector/include"
    )
    target_link_libraries(TESTS
        "C:/Program Files (x86)/Visual Leak Detector/lib/Win64/vld.lib"
    )
endif (MSVC)

target_include_directories(${PROJECT_NAME}
    PRIVATE ${CMAKE_SOURCE_DIR}
)

set(SOURCES "lexer/lexer.hh"
        "lexer/lexer.cc"
        "ast/ast.hh"
        "ast/ast.cc"
        "ast/builtins.cc"
        "parser/parser.hh"
        "parser/parser.cc"
)

target_sources(
    TESTS
    PRIVATE
    ${SOURCES}
    test_lexer.cc
    test_parser.cc
    test_eval.cc
)

target_sources(
    ${PROJECT_NAME}
    PRIVATE
    ${SOURCES}
)
