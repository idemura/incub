default: all

.DEFAULT:
	cd src && $(MAKE) $@

install:
	cd src && $(MAKE) $@

CC=gcc

STD= -std=c99 -pedantic
WARN= -Wall
CFLAGS= $(STD) $(WARN) -g -ggdb

all: noopt

noopt:

baza: types.o main.o
	$(CC) $(CFLAGS)
