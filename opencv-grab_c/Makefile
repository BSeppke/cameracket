MACOSX_CXXFLAGS_32 = -I/src `pkg-config --cflags opencv` -arch i386 -O3
MACOSX_LDFLAGS_32  = `pkg-config --libs opencv` -bundle  -arch i386 -undefined dynamic_lookup

MACOSX_CXXFLAGS_64 = -I/src `pkg-config --cflags opencv` -arch x86_64 -O3
MACOSX_LDFLAGS_64  = `pkg-config --libs opencv` -bundle  -arch x86_64 -undefined dynamic_lookup

UNIX_CXXFLAGS_32 = -I/src `pkg-config --cflags opencv` -m32 -O3 -fPIC
UNIX_LDFLAGS_32  = `pkg-config --libs opencv` -m32 -fPIC 

UNIX_CXXFLAGS_64 = -I/src `pkg-config --cflags opencv` -m64 -O3 -fPIC
UNIX_LDFLAGS_64  = `pkg-config --libs opencv` -m64 -fPIC 
CC = gcc

macosx32:	src/opencv-grab_c.hxx
	cd bin && $(CC) $(MACOSX_CXXFLAGS_32) -c ../src/opencv-grab_c.cxx
	$(CC) $(MACOSX_LDFLAGS_32) bin/opencv-grab_c.o -o bin/libopencv-grab_c.dylib
    
macosx64:	src/opencv-grab_c.hxx
	cd bin && $(CC) $(MACOSX_CXXFLAGS_64) -c ../src/opencv-grab_c.cxx
	$(CC) $(MACOSX_LDFLAGS_64) bin/opencv-grab_c.o -o bin/libopencv-grab_c.dylib

unix32:	src/opencv-grab_c.hxx
	cd bin && $(CC) $(LINUX_CXXFLAGS_32) -c ../src/opencv-grab_c.cxx
	$(CC) bin/opencv-grab_c.o $(LINUX_LDFLAGS_32) -o bin/libopencv-grab_c.so
    
unix64:	src/opencv-grab_c.hxx
	cd bin && $(CC) $(LINUX_CXXFLAGS_64) -c ../src/opencv-grab_c.cxx 
	$(CC) bin/opencv-grab_c.o $(LINUX_LDFLAGS_64) -o bin/libopencv-grab_c.so

clean:
	rm bin/*.*

