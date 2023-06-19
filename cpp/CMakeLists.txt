cmake_minimum_required(VERSION 3.20)

project(
    lexer
    VERSION 0.1.0
    HOMEPAGE_URL "https://github.com/nicdgonzalez/Lexer"
    LANGUAGES CXX
)

add_executable(${PROJECT_NAME} tests.cc)

set_property(
    TARGET ${PROJECT_NAME}
    PROPERTY CXX_STANDARD 23
)
set_property(
    TARGET ${PROJECT_NAME}
    PROPERTY CXX_STANDARD_REQUIRED true
)

target_sources(
    ${PROJECT_NAME}
    PRIVATE
        "lexer/lexer.cc"
)
