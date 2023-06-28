cmake_minimum_required(VERSION 3.20)

project(
    monkey
    VERSION 0.1.0
    HOMEPAGE_URL "https://github.com/nicdgonzalez/Lexer"
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

target_include_directories(TESTS
    PRIVATE ${CMAKE_SOURCE_DIR}
)

target_include_directories(${PROJECT_NAME}
    PRIVATE ${CMAKE_SOURCE_DIR}
)

set(SOURCES "lexer/lexer.hh"
        "lexer/lexer.cc"
        "ast/ast.hh"
        "ast/ast.cc"
        "parser/parser.hh"
        "parser/parser.cc"
)

target_sources(
    TESTS
    PRIVATE
    ${SOURCES}
)

target_sources(
    ${PROJECT_NAME}
    PRIVATE
    ${SOURCES}
)
