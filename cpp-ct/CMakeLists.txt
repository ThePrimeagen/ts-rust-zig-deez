cmake_minimum_required(VERSION 3.15)

# Setting project name 
project(cpp-ct CXX)
# Including cmake settings
include(cmake/StandardProjectSettings.cmake)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Linking Library
add_library(project_options INTERFACE)
add_library(project_warnings INTERFACE)

include_directories(${PROJECT_SOURCE_DIR}/includes)

include(cmake/CompilerWarnings.cmake)
set_project_warnings(project_warnings)

# enable sanitizer options supported by compiler
include(cmake/Sanitizers.cmake)
enable_sanitizers(project_options)

include(cmake/StaticAnalyzers.cmake)

option(ENABLE_TESTING "Enable Test Builds" ON)

option(ENABLE_PCH "Enable Precompiled Header" OFF)
if (ENABLE_PCH)
    target_precompile_headers(project_options INTERFACE <iostream> <vector> <map> <utility>)
endif()

set(CONAN_EXTRA_REQUIRES "")
set(CONAN_EXTRA_OPTIONS "")

include(cmake/Conan.cmake)
run_conan()

if (ENABLE_TESTING)
    enable_testing()
    message(
        "Building Tests."
    )
    add_subdirectory(test)
endif()

add_subdirectory(src)
