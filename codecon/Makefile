# Possible modes: dbg, opt.
MODE=dbg

AR=ar rcs
RM=rm -f
CXX=g++-4.9

ifeq ($(MODE), dbg)
	CXXOPT=-O0 -g
	LDOPT=
else
	CXXOPT=-O3 -ffast-math -DNDEBUG
	LDOPT=-flto
endif
CXXWARNS=-Wall -Wno-unused-function -Wno-sign-compare -Wno-char-subscripts
CXXFLAGS=-std=c++14 -I. -march=native -fdiagnostics-color=auto $(CXXOPT) $(CXXWARNS)

OUTS=*.o *.class *.dSYM

headers:=base.h alg/base.h

.PHONY: clean

sol: sol.cc $(headers)
	$(CXX) $(CXXFLAGS) $< -o $@

run: sol
	./$< < sol.in

clean:
	$(RM) sol $(OUTS) alg/{$(OUTS)}
	find alg/ -type f -executable -exec $(RM) {} \;
