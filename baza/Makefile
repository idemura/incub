default: build_and_test

RM = rm -f
ifeq ($(shell uname), Darwin)
	CC = clang
else
	CC = gcc
endif
DISABLE_WARNINGS = -Wno-unused-function
CFLAGS = -Wall -std=c99 -pedantic $(DISABLE_WARNINGS)
LFLAGS = -Wall
cfg ?= opt
ifeq ($(cfg), debug)
	CFLAGS += -O0 -g -ggdb
else
	CFLAGS += -O2
endif
common=defs.c sds.c btree.c
test_src=test.c main_test.c
out_file=bin/$(cfg)/baza
out_test=bin/$(cfg)/baza_test

build:
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(common) main.c -o $(out_file)
	$(CC) $(CFLAGS) $(common) $(test_src) -o $(out_test) -DTEST

run_test: build
	@$(out_test)

.PHONY: clean
clean:
	@$(RM) -r bin/*
