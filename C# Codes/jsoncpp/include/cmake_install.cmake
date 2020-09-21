# Install script for directory: C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "C:/Program Files/PIME")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "Release")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

if("${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include/json" TYPE FILE FILES
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/allocator.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/assertions.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/autolink.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/config.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/features.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/forwards.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/json.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/reader.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/value.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/version.h"
    "C:/Users/Torsho/Dropbox/Programming Code/C# Codes/PIME/jsoncpp/include/json/writer.h"
    )
endif()

