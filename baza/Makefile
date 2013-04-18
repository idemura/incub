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
	CFLAGS += -O0 -g -ggdb
else
	CFLAGS += -O2 -DNDEBUG
endif
main_src = defs.c dstr.c stack.c pque.c disk.c btree.c main.c
test_src = test.c defs_test.c dstr_test.c stack_test.c pque_test.c disk_test.c btree_test.c main_test.c
main_out = bin/$(cfg)/baza
test_out = bin/$(cfg)/baza_test

main:
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(main_src) -o $(main_out)

test:
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(test_src) -o $(test_out) -DTEST

run_test: test
	@$(test_out)

.PHONY: clean
clean:
	@$(RM) -r bin/*
