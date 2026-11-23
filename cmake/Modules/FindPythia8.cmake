# - Locate pythia8 library
# Provides the imported target:
#
#  Pythia8::Pythia8   (usage: target_link_libraries(tgt PRIVATE Pythia8::Pythia8))
#
# Also defines (legacy variables, kept for compatibility):
#
#  PYTHIA8_FOUND
#  PYTHIA8_VERSION
#  PYTHIA8_INCLUDE_DIR
#  PYTHIA8_XMLDOC_DIR
#  PYTHIA8_INCLUDE_DIRS (not cached)
#  PYTHIA8_LIBRARY
#  PYTHIA8_hepmcinterface_LIBRARY
#  PYTHIA8_lhapdfdummy_LIBRARY
#  PYTHIA8_LIBRARIES (not cached) : includes 3 libraries above; not to be used if lhapdf is used
#
# Search order: PYTHIA8_ROOT_DIR (CMake variable, else environment variable),
# then on macOS the Homebrew prefixes, then the system default.
# Homebrew does not link share/Pythia8/xmldoc into <prefix>/share, so the
# keg (<prefix>/opt/pythia) has to be searched explicitly.
set(_PYTHIA8_ROOTS)
if(PYTHIA8_ROOT_DIR)
  list(APPEND _PYTHIA8_ROOTS ${PYTHIA8_ROOT_DIR})
elseif(DEFINED ENV{PYTHIA8_ROOT_DIR})
  list(APPEND _PYTHIA8_ROOTS $ENV{PYTHIA8_ROOT_DIR})
endif()
if(CMAKE_SYSTEM_NAME MATCHES Darwin)
  if(DEFINED ENV{HOMEBREW_PREFIX})
    list(APPEND _PYTHIA8_ROOTS $ENV{HOMEBREW_PREFIX}/opt/pythia $ENV{HOMEBREW_PREFIX})
  endif()
  list(APPEND _PYTHIA8_ROOTS /opt/homebrew/opt/pythia /usr/local/opt/pythia /opt/homebrew /usr/local)
else()
  list(APPEND _PYTHIA8_ROOTS /usr)
endif()
set(_PYTHIA8_INC_HINTS)
set(_PYTHIA8_XML_HINTS)
set(_PYTHIA8_LIB_HINTS)
foreach(_r ${_PYTHIA8_ROOTS})
  list(APPEND _PYTHIA8_INC_HINTS ${_r}/include)
  list(APPEND _PYTHIA8_XML_HINTS ${_r}/xmldoc
                                 ${_r}/share/Pythia8/xmldoc
                                 ${_r}/share/pythia8-data/xmldoc
                                 ${_r}/share/doc/packages/pythia/xmldoc)
  list(APPEND _PYTHIA8_LIB_HINTS ${_r}/lib ${_r}/lib64)
endforeach()

find_path(PYTHIA8_INCLUDE_DIR Pythia.h Pythia8/Pythia.h
  HINTS  ${_PYTHIA8_INC_HINTS})

find_path(PYTHIA8_XMLDOC_DIR Version.xml
  HINTS  ${_PYTHIA8_XML_HINTS})

if(PYTHIA8_INCLUDE_DIR AND PYTHIA8_XMLDOC_DIR)
  file(READ ${PYTHIA8_XMLDOC_DIR}/Version.xml versionstr)
  string(REGEX REPLACE ".*Pythia:versionNumber.*default.*[0-9][.]([0-9]+).*" "\\1" PYTHIA8_VERSION "${versionstr}")
  set(PYTHIA8_VERSION "8.${PYTHIA8_VERSION}")
  find_library(PYTHIA8_LIBRARY NAMES pythia8 Pythia8
    HINTS ${_PYTHIA8_LIB_HINTS})

  find_library(PYTHIA8_lhapdfdummy_LIBRARY NAMES lhapdfdummy
    HINTS ${_PYTHIA8_LIB_HINTS})

  set(PYTHIA8_INCLUDE_DIRS ${PYTHIA8_INCLUDE_DIR} ${PYTHIA8_INCLUDE_DIR}/Pythia8 )
  set(PYTHIA8_LIBRARIES ${PYTHIA8_LIBRARY})
  if(PYTHIA8_VERSION VERSION_LESS 8.200)
    #Is this library needed?
    set(PYTHIA8_LIBRARIES ${PYTHIA8_LIBRARY} ${PYTHIA8_lhapdfdummy_LIBRARY})
  endif()
endif()

# handle the QUIETLY and REQUIRED arguments and set PYTHIA8_FOUND to TRUE if
# all listed variables are TRUE

INCLUDE(FindPackageHandleStandardArgs)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(Pythia8 REQUIRED_VARS PYTHIA8_INCLUDE_DIR PYTHIA8_LIBRARIES PYTHIA8_XMLDOC_DIR VERSION_VAR PYTHIA8_VERSION )

if(PYTHIA8_FOUND AND NOT TARGET Pythia8::Pythia8)
  add_library(Pythia8::Pythia8 UNKNOWN IMPORTED)
  set_target_properties(Pythia8::Pythia8 PROPERTIES
    IMPORTED_LOCATION "${PYTHIA8_LIBRARY}"
    INTERFACE_INCLUDE_DIRECTORIES "${PYTHIA8_INCLUDE_DIR};${PYTHIA8_INCLUDE_DIR}/Pythia8")
  if(PYTHIA8_VERSION VERSION_LESS 8.200 AND PYTHIA8_lhapdfdummy_LIBRARY)
    set_property(TARGET Pythia8::Pythia8 APPEND PROPERTY INTERFACE_LINK_LIBRARIES "${PYTHIA8_lhapdfdummy_LIBRARY}")
  endif()
endif()

mark_as_advanced(PYTHIA8_FOUND PYTHIA8_INCLUDE_DIR PYTHIA8_LIBRARY PYTHIA8_LIBRARIES PYTHIA8_XMLDOC_DIR)

