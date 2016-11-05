########################################
# Set up how to compile the source files
########################################

# Add the source files
SET(MCJADE_src ${SRCMCJADE}/mcjade.f ${SRCMCJADE}/mcmain.f  ${SRCMCJADE}/mcrand.f  ${SRCMCJADE}/mcuser.f ${SRCSUPERV}/showb.f)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${MCJADEEXE} ${MCJADE_src})

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links foo to the bar library
#jmc -lboslib -ljadegs -linterface -ljademus -lsource -lwertex -lpatrecsr 
TARGET_LINK_LIBRARIES(${MCJADEEXE} -Wl,--start-group ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR} 
${CERN_LIBRARIES} X11 -Wl,--end-group)
#${CERNLIB_LIBRARY_DIR}/
# Uncomment if you need to link to BLAS and LAPACK
#TARGET_LINK_LIBRARIES(${MCJADEEXE} ${BLAS_LIBRARIES}
#                                ${LAPACK_LIBRARIES}
#                                ${CMAKE_THREAD_LIBS_INIT})

#####################################
# Tell how to install this executable
#####################################

IF(WIN32)
    SET(CMAKE_INSTALL_PREFIX "C:\\Program Files")
ELSE()
    SET(CMAKE_INSTALL_PREFIX /usr/local)
ENDIF(WIN32)
INSTALL(TARGETS ${MCJADEEXE} RUNTIME DESTINATION bin)
