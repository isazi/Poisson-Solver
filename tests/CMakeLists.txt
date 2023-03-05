add_test(
    NAME end-to-end-run
    COMMAND ${CMAKE_BINARY_DIR}/bin/solver 50 10000
    WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
    )

file(GLOB OUTPUTS "${CMAKE_BINARY_DIR}/../tests/test-data/fort.*")
foreach(output ${OUTPUTS})
    get_filename_component(fortfile ${output} NAME)
    add_test(
        NAME "verify-result-${fortfile}"
        COMMAND numdiff -S -a 1e-5 ${fortfile} ../tests/test-data/${fortfile}
        WORKING_DIRECTORY "${CMAKE_BINARY_DIR}"
        )
endforeach()
