default: sol

RM = rm -f
ifeq ($(shell uname), Darwin)
	CC = clang
else
	CC = gcc
endif
DISABLE_WARNINGS =
STD ?= gnu89
CFLAGS = -Wall -std=$(STD) -march=native $(DISABLE_WARNINGS) -D_GNU_SOURCE
LFLAGS = -Wall
cfg ?= debug
ifeq ($(cfg), debug)
	CFLAGS += -O0 -g -ggdb
else
	CFLAGS += -O2 -flto -ffast-math -DNDEBUG
endif
sol_src = sol.c rb.c
sol_out = sol.out
lib_src = lib.c
lib_out = lib.out

sol:
	$(CC) $(CFLAGS) $(sol_src) -o $(sol_out)

lib:
	$(CC) $(CFLAGS) $(lib_src) -o $(lib_out)

.PHONY: clean
clean:
	@$(RM) *.o *.out
