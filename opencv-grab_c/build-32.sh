#!/bin/bash
rm -rf ./build
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS=-m32 -DCMAKE_CXX_FLAGS=-m32 ..
make 
cd ..
rm -rf ./build
