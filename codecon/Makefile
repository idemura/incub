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
cfg ?= debug
ifeq ($(cfg), debug)
	CFLAGS += -O0 -g -ggdb
else
	CFLAGS += -O2 -DNDEBUG
endif
main_src = defs.c main.c
out_main = main.out

main:
	$(CC) $(CFLAGS) $(main_src) -o $(out_main)

run_main: main
	@./$(out_main)

.PHONY: clean
clean:
	@$(RM) *.o *.out
