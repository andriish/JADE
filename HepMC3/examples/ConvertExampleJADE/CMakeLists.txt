

if(ROOT_FOUND)
include_directories(${PROJECT_SOURCE_DIR}/rootIO/include ${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR}  ${ROOT_INCLUDE_DIRS})

#----------------------------------------------------------------------------
# Generate dictionaries
#


add_executable(convert_example_JADE.exe
        ${CMAKE_CURRENT_SOURCE_DIR}/convert_example.cc
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterRootTreeOPAL.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterHEPEVTZEUS.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterJADE.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/jadeio.f
        )
target_link_libraries(convert_example_JADE.exe ${ROOT_LIBRARIES} HepMC HepMCrootIO lapack)
set_target_properties(convert_example_JADE.exe PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR})
add_dependencies( convert_example_JADE.exe dictionary )


# create environment scripts

set(bindir "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}")
set(libdir "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}:${ROOT_LIBRARY_DIR}")
else()
include_directories(${PROJECT_SOURCE_DIR}/rootIO/include ${CMAKE_CURRENT_SOURCE_DIR}/include ${CMAKE_CURRENT_SOURCE_DIR})
#----------------------------------------------------------------------------
# Generate dictionaries
#
add_executable(convert_example_JADE.exe
        ${CMAKE_CURRENT_SOURCE_DIR}/convert_example.cc
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterHEPEVTZEUS.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/WriterJADE.cc 
        ${CMAKE_CURRENT_SOURCE_DIR}/src/jadeio.f
        )
target_link_libraries(convert_example_JADE.exe HepMC )
set_target_properties(convert_example_JADE.exe PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR})
add_dependencies( convert_example_JADE.exe dictionary )
# create environment scripts
set(bindir "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_BINDIR}")
set(libdir "${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}")
endif()
 
configure_file(
${CMAKE_SOURCE_DIR}/examples/ConvertExampleJADE/convert_example_env.sh.in
	    ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR}/convert_example_env.sh
     	     @ONLY
      	     )

# installs 
install(TARGETS convert_example_JADE.exe DESTINATION ${CMAKE_INSTALL_BINDIR})
install(DIRECTORY ${PROJECT_BINARY_DIR}/examples/ConvertExample_JADE/ DESTINATION ${CMAKE_INSTALL_BINDIR} FILES_MATCHING PATTERN "*.pcm" PATTERN "CMakeFiles" EXCLUDE )
install(DIRECTORY ${PROJECT_BINARY_DIR}/outputs/${CMAKE_INSTALL_BINDIR}/ DESTINATION ${CMAKE_INSTALL_BINDIR}
        FILES_MATCHING PATTERN "*.sh")
