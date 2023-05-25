include(FetchContent)

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        release-1.11.0
)
FetchContent_MakeAvailable(googletest)
add_library(GTest::GTest INTERFACE IMPORTED)
target_link_libraries(GTest::GTest INTERFACE gtest_main)

add_executable(lexer_test lexer_test.cpp)

target_link_libraries(lexer_test
 PRIVATE
  GTest::GTest
  TsRustZigDeez_lib)

add_test(NAME lexer_test COMMAND lexer_test)
