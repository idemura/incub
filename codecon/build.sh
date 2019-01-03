#! /bin/bash

OS_NAME=`uname`
if [[ "$OS_NAME" == "Darwin" ]]; then
  CXX=clang++
else
  CXX=g++
fi
CXX_CODEGEN='-O0 -g -fsanitize=address -fno-omit-frame-pointer'
# CXX_CODEGEN='-O3 -ffast-math -flto -DNDEBUG'

BINARY=${1%.*}
$CXX -std=c++17 -I. -march=native $1 -o ${BINARY} \
    ${CXX_CODEGEN} \
    ${CXX_COMPILER_SPECIFIC} \
    -fdiagnostics-color=auto \
    -fno-exceptions \
    -fno-rtti \
    -Wall -Wshadow -Wconversion -Wliteral-range \
    -Wno-unused-function -Wno-sign-conversion -Wno-sign-compare -Wno-char-subscripts \
    -lgtest \
    -lgmock \
    -lglog \
    -lgflags \
    || exit

shift
[[ $1 =~ ^-r|--run$ ]] || exit
shift
[[ $1 == "" ]] && IN="/dev/null" || IN="$1"
./${BINARY} < $IN
