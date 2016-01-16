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
CXXWARNS=-Wall -Wno-unused-function -Wno-sign-compare -Wno-char-subscripts
CXXFLAGS=-std=c++14 -I. -march=native -fdiagnostics-color=auto $(CXXOPT) $(CXXWARNS)
.PHONY: clean
all: iped
iped.o: libgeneric.a libmongoose.a iped.hxx
	$(CXX) $(CXXFLAGS) -c iped.cxx
base_test: base_test.o
	$(CXX) $(LDOPT) -L. -o base_test base.o base_test.o -lcityhash
libgeneric.a: base.o template.o
	$(AR) libgeneric.a template.o base.o
base.o: base.hxx
	$(CXX) $(CXXFLAGS) -c base.cxx
libmongoose.a: mongoose.c mongoose.h
	$(CC) -I. -march=native $(CXXOPT) -c mongoose.c -DMONGOOSE_NO_FILE_SYSTEM
	$(AR) libmongoose.a mongoose.o
iped: iped.o
	$(CXX) $(LDOPT) -L. -o iped iped.o -lgeneric -lcityhash -lmongoose -lpthread
base_test.o: base.o
	$(CXX) $(CXXFLAGS) -c base_test.cxx
template_test: template_test.o
	$(CXX) $(LDOPT) -L. -o template_test base.o template.o template_test.o -lcityhash
template.o: base.o template.hxx
	$(CXX) $(CXXFLAGS) -c template.cxx
template_test.o: template.o
	$(CXX) $(CXXFLAGS) -c template_test.cxx
clean:
	$(RM) *.o base_test libgeneric.a libmongoose.a iped base_test template_test template_test
tests: base_test template_test
	./base_test
	./template_test
	@echo TESTS PASSED
