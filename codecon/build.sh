#! /bin/bash

OS_NAME=`uname`
if [[ "$OS_NAME" == "Darwin" ]]; then
  CXX=clang++
  CXX_SPECIFIC=-Wliteral-range
else
  CXX=g++
  CXX_SPECIFIC=
fi
if [[ ${OPT} ]]; then
    CXX_CODEGEN='-O3 -ffast-math -flto -DNDEBUG'
else
    CXX_CODEGEN='-O0 -g -fsanitize=address -fno-omit-frame-pointer'
fi

BINARY=${1%.*}
$CXX -std=c++17 -I. -march=native $1 -o ${BINARY} \
    ${CXX_CODEGEN} \
    ${CXX_COMPILER_SPECIFIC} \
    -fdiagnostics-color=auto \
    -fno-exceptions \
    -fno-rtti \
    -Wall -Wshadow -Wconversion \
    -Wno-unused-function -Wno-sign-conversion -Wno-sign-compare -Wno-char-subscripts \
    ${CXX_SPECIFIC} \
    -lgtest \
    -lgmock \
    -lglog \
    -lgflags \
    -pthread \
    || exit

shift
[[ $1 =~ ^-r|--run$ ]] || exit
shift
[[ $1 == "" ]] && IN="/dev/null" || IN="$1"
./${BINARY} < $IN
