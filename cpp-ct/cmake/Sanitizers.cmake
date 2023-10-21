function(enable_sanitizers project_name)

    if (CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR CMAKE_CXX_COMPILER_ID STREQUAL "Clang")

        option(ENABLE_COVERAGE "Enable coverage reporting for gcc/clang" OFF)
        if (ENABLE_COVERAGE)
            target_compile_options(${project_name} INTERFACE --coverage -O0 -g)
            target_link_libraries(${project_name} INTERFACE --coverage)
        endif()

        set(SANITIZERS "")

        option(ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" OFF)
        if (ENABLE_SANITIZER_ADDRESS)
            list(APPEND SANITIZERS "address")
        endif()

        option(ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
        if (ENABLE_SANITIZER_MEMORY)
            list(APPEND SANITIZERS "memory")
        endif()

        option(ENABLE_SANITIZER_UNDEFINED_BEHAVIOR "Enable undefined behavior sanitizer" OFF)
        if (ENABLE_SANITIZER_UNDEFINED_BEHAVIOR)
            list(APPEND SANITIZERS "undefined")
        endif()

        option(ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
        if (ENABLE_SANITIZER_THREAD)
            list(APPEND SANITIZERS "thread")
        endif()

        list(JOIN SANITIZERS "," LIST_OF_SANITIZERS)
        
    endif()
    
    if (LIST_OF_SANITIZERS)
        
        if (NOT "${LIST_OF_SANITIZERS}" STREQUAL "")

            target_compile_options(${project_name}
                INTERFACE -fsanitizers=${LIST_OF_SANITIZERS}
            )

            target_link_libraries(${project_name}
                INTERFACE -fsanitizers=${LIST_OF_SANITIZERS}
            )

        endif()

    endif()


endfunction(enable_sanitizers project_name)
