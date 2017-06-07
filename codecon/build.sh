#! /bin/bash

OS_NAME=`uname`
if [[ "$OS_NAME" == "Darwin" ]]; then
  CXX=clang++
else
  CXX=g++
fi
CXX_CODEGEN='-O0 -g'
# CXX_CODEGEN='-O3 -ffast-math -flto -DNDEBUG'

$CXX -std=c++14 -I. -march=native $1 -o ${1%.*} \
    ${CXX_CODEGEN} \
    -fdiagnostics-color=auto \
    -Wall -Wno-unused-function -Wno-sign-compare -Wno-char-subscripts
