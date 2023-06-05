include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        release-1.11.0
)
FetchContent_MakeAvailable(googletest)
add_library(GTest::GTest INTERFACE IMPORTED)
target_link_libraries(GTest::GTest INTERFACE gtest_main)

add_executable(tests
  lexer_test.cpp
  parser_test.cpp
)


add_compile_options(
  -Wall -Wpedantic
)

target_include_directories(tests
 PUBLIC
  src
)

target_link_libraries(tests
 PRIVATE
  GTest::GTest
  TsRustZigDeez_lib)

add_test(NAME tests COMMAND tests)
