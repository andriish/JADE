########################################
# Set up how to compile the source files
########################################

# Add the source files
SET(JADEZ_src ${SRCJADEZ}/gphmain.F  ${SRCJADEZ}/xuser.F
)

# Define the executable in terms of the source files
ADD_EXECUTABLE(${JADEZEXE} ${JADEZ_src})

#####################################################
# Add the needed libraries and special compiler flags
#####################################################

# This links foo to the bar library
#-lpatrecsr -ltoflib -ltagg -lboslib -ljadegs -linterface -ljademus -lsource -lwertex -lpatrecsr
TARGET_LINK_LIBRARIES(${JADEZEXE}  -Wl,--start-group ${GRAFIX} ${BOSLIB} ${TOFLIB} ${TAGG} ${JADEGS} ${INTERFACE} ${JADEMUS} ${SOURCE} ${WERTEX} ${PATRECSR}
 ${CERN_LIBRARIES} X11 gfortran -Wl,--end-group)

# Uncomment if you need to link to BLAS and LAPACK
#TARGET_LINK_LIBRARIES(${JADEZEXE} ${BLAS_LIBRARIES}
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
INSTALL(TARGETS ${JADEZEXE} RUNTIME DESTINATION bin)
