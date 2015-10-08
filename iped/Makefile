# Possible modes: dbg, opt.
MODE=dbg

RM=rm -f

CXX=g++-4.9
CC=gcc
ifeq ($(MODE), dbg)
	CXXOPT=-O0 -g
	LDOPT=
else
	CXXOPT=-O3 -ffast-math -DNDEBUG
	LDOPT=-flto
endif
CXXWARNINGS=-Wall -Wno-unused-function -Wno-sign-compare -Wno-char-subscripts
CXXFLAGS=-std=c++14 -I. -march=native -fdiagnostics-color=auto $(CXXOPT) \
				 $(CXXWARNINGS)

headers:=$(wildcard *.hxx)
sources:=$(wildcard *.cxx)
objects:=$(filter-out %_test.cxx, $(sources))

all: iped tests

tests: flags_test

flag_test: $(objects) $(headers) flag_test.o
	$(CXX) -o  $(objects) mongoose.o $(LDOPT)

clean:
	@$(RM) *.o iped

iped: $(objects) $(headers) mongoose.o
	$(CXX) -o iped $(objects) mongoose.o $(LDOPT)

%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $<

mongoose.o: mongoose.c
	$(CC) -I. -march=native $(CXXOPT) -c $<

