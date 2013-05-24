default: sol

RM = rm -f
ifeq ($(shell uname), Darwin)
	CC = clang
else
	CC = gcc
endif
DISABLE_WARNINGS = -Wno-unused-function -Wno-unused-result
STD ?= gnu89
CFLAGS = -Wall -std=$(STD) -march=native $(DISABLE_WARNINGS) -DDEBUG
cfg ?= debug
ifeq ($(cfg), debug)
	CFLAGS += -O0 -g
else
	CFLAGS += -O2 -flto -ffast-math -DNDEBUG
endif
sol_src = sol.c

sol:
	$(CC) $(CFLAGS) $(sol_src) -o sol.out -lm

.PHONY: clean
clean:
	@$(RM) *.o *.out
