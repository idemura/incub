default: main

RM = rm -f
ifeq ($(shell uname), Darwin)
	CC = clang
else
	CC = gcc
endif
DISABLE_WARNINGS = -Wno-unused-function
CFLAGS = -Wall -std=c99 -march=native -pedantic $(DISABLE_WARNINGS)
LFLAGS = -Wall
cfg ?= opt
ifeq ($(cfg), debug)
	CFLAGS += -O0 -g -ggdb
else
	CFLAGS += -O2 -flto -ffast-math -DNDEBUG
endif
main_src = scgilib.c defs.c dstr.c main.c
main_out = bin/$(cfg)/cweb

main:
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(main_src) -o $(main_out)

.PHONY: clean
clean:
	@$(RM) -r bin/*
