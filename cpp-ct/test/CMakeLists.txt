# automatically enable catch2 to generate ctest targets
if(CONAN_CATCH2_ROOT_DEBUG)
  include(${CONAN_CATCH2_ROOT_DEBUG}/lib/cmake/Catch2/Catch.cmake)
else()
  include(${CONAN_CATCH2_ROOT}/lib/cmake/Catch2/Catch.cmake)
endif()


add_library(catch_main STATIC catch_main.cpp)
target_link_libraries(catch_main PUBLIC CONAN_PKG::catch2)

if ( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/runtime/test_list.cmake" )
  include("${CMAKE_CURRENT_SOURCE_DIR}/runtime/test_list.cmake")
  
  foreach(TEST_FILE ${RUNTIME_TEST})
    
    string( REPLACE ".cpp" "" test_name ${TEST_FILE} )
    add_executable( "${test_name}" "${CMAKE_CURRENT_SOURCE_DIR}/runtime/${TEST_FILE}")
    target_link_libraries( "${test_name}" PRIVATE project_warnings project_options
                                    catch_main)

    # automatically discover tests that are defined in catch based test files you
    # can modify the unittests. TEST_PREFIX to whatever you want, or use different
    # for different binaries
    catch_discover_tests(
      "${test_name}"
      TEST_PREFIX
      "unittests."
      EXTRA_ARGS
      -s
      --reporter=xml
      --out=tests.xml)

  endforeach(TEST_FILE ${RUNTIME_TEST})

endif()

if ( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/constexpr/test_list.cmake" )
  include("${CMAKE_CURRENT_SOURCE_DIR}/constexpr/test_list.cmake")

  foreach(TEST_FILE ${CONSTEXPR_TEST})
    
    string( REPLACE ".cpp" "" test_name ${TEST_FILE} )
    add_executable( "${test_name}" "${CMAKE_CURRENT_SOURCE_DIR}/constexpr/${TEST_FILE}")
    target_link_libraries( "${test_name}" PRIVATE project_warnings project_options
                                    catch_main)

    catch_discover_tests(
      "${test_name}"
      TEST_PREFIX
      "constexpr."
      EXTRA_ARGS
      -s
      --reporter=xml
      --out=constexpr.xml)

  endforeach(TEST_FILE ${CONSTEXPR_TEST})

endif()

if ( EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/constexpr/test_list.cmake" )
  include("${CMAKE_CURRENT_SOURCE_DIR}/constexpr/test_list.cmake")
  
  foreach(TEST_FILE ${CONSTEXPR_TEST})
  
    string( REPLACE ".cpp" "" test_name ${TEST_FILE} )
    add_executable( "relaxed_${test_name}" "${CMAKE_CURRENT_SOURCE_DIR}/constexpr/${TEST_FILE}")

    target_link_libraries(
      "relaxed_${test_name}" PRIVATE project_options project_warnings
                                  catch_main)

    target_compile_definitions(
      "relaxed_${test_name}" PRIVATE
      -DCATCH_CONFIG_RUNTIME_STATIC_REQUIRE)

    catch_discover_tests(
      "relaxed_${test_name}"
      TEST_PREFIX
      "relaxed_constexpr."
      EXTRA_ARGS
      -s
      --reporter=xml
      --out=constexpr.xml)

  endforeach(TEST_FILE ${CONSTEXPR_TEST})

endif()

# # Disable the constexpr portion of the test, and build again this allows us to
# # have an executable that we can debug when things go wrong with the constexpr
# # testing
# add_executable(relaxed_constexpr_tests constexpr_tests.cpp)
# target_link_libraries(
#   relaxed_constexpr_tests PRIVATE project_options project_warnings
#                                   catch_main)
# target_compile_definitions(
#   relaxed_constexpr_tests PRIVATE
#                                   -DCATCH_CONFIG_RUNTIME_STATIC_REQUIRE)


# catch_discover_tests(
#   relaxed_constexpr_tests
#   TEST_PREFIX
#   "relaxed_constexpr."
#   EXTRA_ARGS
#   -s
#   --reporter=xml
#   --out=relaxed_constexpr.xml)