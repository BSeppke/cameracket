cmake_minimum_required(VERSION 3.1)

project(opencv-grab_c)

#find . -type f -name \*.cxx | sed 's,^\./,,'
set(SOURCES 
	opencv-grab_c.cxx)

#find . -type f -name \*.hxx | sed 's,^\./,,'
set(HEADERS  
	opencv-grab_c.hxx
	os_settings.hxx)

# Tell CMake to create the library
add_library(opencv-grab_c SHARED ${SOURCES} ${HEADERS})
target_link_libraries(opencv-grab_c ${OpenCV_LIBS})