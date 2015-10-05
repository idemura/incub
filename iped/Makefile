# Possible modes: dbg, opt.
MODE=dbg

RM=rm -f

CXX=g++-4.9
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
objects:=$(patsubst %.cxx,%.o,$(sources))

%.o: %.cxx
	$(CXX) $(CXXFLAGS) -c $<

iped: $(objects) $(headers)
	$(CXX) -o iped $(objects) $(LDOPT)

all: iped

clean:
	@$(RM) *.o iped

