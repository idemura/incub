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
iped.o: base.o flags.o template.o libmongoose.a iped.hxx
	$(CXX) $(CXXFLAGS) -c iped.cxx
template_test.o: template.o
	$(CXX) $(CXXFLAGS) -c template_test.cxx
base_test: base_test.o
	$(CXX) $(LDOPT) -L. -o base_test base.o base_test.o -lcityhash
base.o: base.hxx
	$(CXX) $(CXXFLAGS) -c base.cxx
libmongoose.a: mongoose.c mongoose.h
	$(CC) -I. -march=native $(CXXOPT) -c mongoose.c -DMONGOOSE_NO_FILE_SYSTEM
	$(AR) libmongoose.a mongoose.o
iped: iped.o
	$(CXX) $(LDOPT) -L. -o iped base.o template.o iped.o flags.o -lmongoose -lcityhash -lpthread
base_test.o: base.o
	$(CXX) $(CXXFLAGS) -c base_test.cxx
flags_test.o: flags.o
	$(CXX) $(CXXFLAGS) -c flags_test.cxx
template_test: template_test.o
	$(CXX) $(LDOPT) -L. -o template_test base.o template.o template_test.o -lcityhash
flags_test: flags_test.o
	$(CXX) $(LDOPT) -L. -o flags_test base.o flags_test.o flags.o -lcityhash
template.o: base.o template.hxx
	$(CXX) $(CXXFLAGS) -c template.cxx
flags.o: base.o flags.hxx
	$(CXX) $(CXXFLAGS) -c flags.cxx
clean:
	$(RM) iped.o template_test.o template_test base_test base.o libmongoose.a iped base_test.o base_test flags_test.o flags_test template_test flags_test template.o flags.o
tests: base_test flags_test template_test
	./base_test
	./flags_test
	./template_test
	@echo TESTS PASSED
