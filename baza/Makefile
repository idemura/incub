default: server

RM = rm -f
ifeq ($(shell uname), Darwin)
	CC = clang
else
	CC = gcc
endif
CFLAGS = -Wall -std=c99 -pedantic
LFLAGS = -Wall
cfg ?= opt
ifeq ($(cfg), debug)
	CFLAGS += -O0 -g -ggdb
else
	CFLAGS += -O2
endif
sources=$(wildcard *.c)
outfile=bin/$(cfg)/baza

debug:
	@$(MAKE) server cfg="debug"

server: $(objects)
	@mkdir -p bin/$(cfg)
	$(CC) $(CFLAGS) $(sources) -o $(outfile)

.PHONY: clean
clean:
	$(RM) -r bin/*
