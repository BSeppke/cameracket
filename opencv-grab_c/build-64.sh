#!/bin/bash
rm -rf ./build
mkdir build
cd build
cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_FLAGS=-m64 -DCMAKE_CXX_FLAGS=-m64 ..
make 
cd ..
rm -rf ./build
