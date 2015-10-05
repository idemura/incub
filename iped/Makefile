MODE=dbg

RM=rm -f

CXX=g++-4.9
ifeq ($(MODE), dbg)
	CXXOPT=O0 -g -DDEBUG
	LDOPT=
else
	CXXOPT=O3 -ffast-math -DNDEBUG
	LDOPT=-flto
endif
CXXWARNINGS=-Wall -Wno-unused-function -Wno-sign-compare -Wno-char-subscripts
CXXFLAGS=-std=c++14 -I. -march=native -fdiagnostics-color=auto \
				 $(CXXWARNINGS) $(CXXOPT)

headers:=$(wildcard *.cxx)
objects:=$(patsubst %.cxx,%.o,$(wildcard *.cxx))

iped: $(objects) $(headers)
	$(CXX) -o iped $(objects) $(LDOPT)

all: iped

clean:
	$(RM) *.o iped

