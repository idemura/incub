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
sol_src = sol.c rb.c
sol_out = sol.out

main:
	$(CC) $(CFLAGS) $(sol_src) -o $(sol_out)

run_main: main
	@./$(sol_out)

.PHONY: clean
clean:
	@$(RM) *.o *.out
