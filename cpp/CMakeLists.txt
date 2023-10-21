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
        "parser/parser.hh"
        "parser/parser.cc"
        "ast/object.hh"
        "ast/object.cc"
        "ast/ast.hh"
        "ast/ast.cc"
        "ast/builtins.cc"
        "vm/code.hh"
        "vm/code.cc"
        "vm/compiler.hh"
        "vm/compiler.cc"
        "vm/vm.hh"
        "vm/vm.cc"
        "vm/symbol_table.hh"
        "vm/symbol_table.cc"
)

target_sources(
    TESTS
    PRIVATE
    ${SOURCES}
    test_lexer.cc
    test_parser.cc
    test_eval.cc
    test_code.cc
    test_vm.cc
    test_symbol_table.cc
)

target_sources(
    ${PROJECT_NAME}
    PRIVATE
    ${SOURCES}
)
