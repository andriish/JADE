# Arguments:
#  PREFIX    - Installation prefix
#  COMPONENTS - Installation component
  
set(COMPONENTS ${COMPONENTS})
foreach(component ${COMPONENTS})
  message(STATUS "component ${component} prefix ${PREFIX}")
  execute_process(
    COMMAND ${CMAKE_COMMAND} -DCMAKE_INSTALL_PREFIX=${PREFIX}
                             -DCMAKE_INSTALL_COMPONENT=${component}
                             -P cmake_install.cmake
    RESULT_VARIABLE result
    OUTPUT_QUIET
  )

  if(result)
    set(msg "Local installation of ${component} failed: ${result}\n")
    message(FATAL_ERROR "${msg}")
  endif()
endforeach()
