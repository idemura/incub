default: main

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
	CFLAGS += -O0 -g -ggdb -DDEBUG
else
	CFLAGS += -O2
endif
main_src=defs.c dstr.c btree.c pque.c main.c
test_src=test.c dstr_test.c btree_test.c pque_test.c main_test.c
out_main=bin/$(cfg)/baza
out_test=bin/$(cfg)/baza_test

main:
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(main_src) -o $(out_main)

test:
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(test_src) -o $(out_test) -DTEST

run_test: test
	@$(out_test)

.PHONY: clean
clean:
	@$(RM) -r bin/*
