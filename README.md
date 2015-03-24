cameracket
==========

A lightweight extension to the functional programming language Racket (formerly Scheme) which allows to grab images from an attaches (web-)camera. Uses OpenCV for the grabbing part and stores the results by means of a vigracket image.

1. Prerequisites
-----------------------------------

For Linux and Mac OS X, the OpenCV Computer Vision library needs to be installed. I recommend the use of a current version. The easiest way to do so, is using your favorite package manager under linux or using MacPorts und  Mac OS X. Otherwise you need to pay attention to install all the needed dependencies on your own.

After the installation, the command 
> pkg-config --modversion opencv
needs to find the opencv-bindings for Linux and MacOS

Note, that for Windows, you also need to have installed the MS VC-Runtime (2010) in order to get these binaries running.
 
2. Installation
-----------------------------------

The installation of the cameracket-bindings is quite easy. Just unzip the release package, if you have downloaded a release package.

Inside the vigracket-directory you will find a file called "install.rkt". Open this file in DrRacket and execute it once. This will copy the cameracket files to the local collects directory and start the auto-build of the opencv-bindings. This should build the wrapper library under Linux and Mac OS X or copy the correct binaries for Windows.

If this does not run out-of the box, you may need to change the systems path-variable in order to find the vigra library. However, if the vigra installation is in the standard system path, you can simply replace line 41 in file "install.rkt" with

> (define (system-env arg) (system arg))

and then start to run the examples again. If this doesn't work either, look for a built binary file at "cameracket/opencv-grab_c/bin/libopencv-grab_c.so". Under some circumstances, the rights may not suffice for Racket to copy it to the cameracket directory. In case, please do that on your own.

3. Using the cameracket module
----------------------------------

After successful installation, you can include the package by calling
> (require cameracket)

You should be able to run the examples provided by means of loading the "examples.rkt" file into DrRacket and pressing the "Run" button.