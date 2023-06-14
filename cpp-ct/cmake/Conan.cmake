macro(run_conan)

    if (NOT EXISTS "${CMAKE_BINARY_DIR}/conan.cmake")
        message(
            STATUS
            "Downloading conan.cmake from https://github.com/conan-io/cmake-conan"
        )
        file(DOWNLOAD "https://raw.githubusercontent.com/conan-io/cmake-conan/0.18.1/conan.cmake"
            "${CMAKE_BINARY_DIR}/conan.cmake"
        )
    endif()

    include("${CMAKE_BINARY_DIR}/conan.cmake")

    conan_add_remote(NAME conan-center URL
        https://center.conan.io
    )

    conan_cmake_run(
        REQUIRES
        ${CONAN_EXTRA_REQUIRES}
        catch2/3.3.2
        OPTIONS
        ${CONAN_EXTRA_OPTIONS}
        BASIC_SETUP
        CMAKE_TARGETS # individual targets to link to
        BUILD
        missing
    )

endmacro()