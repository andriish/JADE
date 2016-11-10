########################################
# Set up how to compile the source files
########################################

# Add the source files
SET(ZE4V_src ${SRCZE4V}/ze4vjb.f ${SRCMCJADE}/mcrand.f ${SRCSUPERV}/showb.f )

# Define the executable in terms of the source files
ADD_EXECUTABLE(${ZE4VEXE} ${ZE4V_src})

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links foo to the bar library
TARGET_LINK_LIBRARIES(${ZE4VEXE} -Wl,--start-group ${ZLIB} ${JMC} ${BOSLIB} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
 ${CERN_LIBRARIES} -Wl,--end-group)

# Uncomment if you need to link to BLAS and LAPACK
#TARGET_LINK_LIBRARIES(${ZE4VEXE} ${BLAS_LIBRARIES}
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
INSTALL(TARGETS ${ZE4VEXE} RUNTIME DESTINATION bin)
