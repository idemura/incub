# Possible modes: dbg, opt.
MODE=dbg

RM=rm -f
AR=ar rcs
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
LDLIB=-lpthread -lcityhash

sources:=base.cxx flags.cxx template.cxx
headers:=base.hxx flags.hxx template.hxx
objects:=$(patsubst %.cxx,%.o,$(sources))
tests:=$(patsubst %_test.cxx,%_test,$(wildcard *_test.cxx))

.PHONY: clean

all: iped $(tests)
run_tests: all
	./base_test
	./flags_test
	./template_test
	@echo TESTS PASSED

clean:
	$(RM) *.o iped *_test

iped: iped.o mongoose.o $(objects)
	$(CXX) $(LDOPT) -o $@ $^ $(LDLIB)

%_test: %_test.o $(objects)
	$(CXX) $(LDOPT) -o $@ $^ $(LDLIB)

%.o: %.cxx $(headers)
	$(CXX) $(CXXFLAGS) -c $<

mongoose.o: mongoose.c mongoose.h
	$(CC) -I. -march=native $(CXXOPT) -c $< -DMONGOOSE_NO_FILESYSTEM

