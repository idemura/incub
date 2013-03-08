default: all

CC=gcc

STD= -std=c99 -pedantic
WARN= -Wall
CFLAGS= $(STD) $(WARN) -g -ggdb

all: debug

debug: baza

baza: types.o main.o
	$(CC) $(CFLAGS) types.c main.c
