default: server

CC=gcc
OPT?=-O2
DBG?=
CFLAGS=-Wall -std=c99 -pedantic
LFLAGS=-Wall
INCLUDES=
LIBS=
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)
NAME=baza

#opt:
#	$(MAKE) OPT="-O2" DBG=""

#debug:
#	$(MAKE) OPT="-O0" DBG="-g -ggdb"

%.o: %.c
	$(CC) $(CFLAGS) $(OPT) $(DBG) $(INCLUDES) -c $< -o $@

server: $(OBJS)
	$(CC) $(LFLAGS) $(LIBS) $(OBJS) -o $(NAME)

.PHONY: clean

clean:
	rm $(NAME)
	rm *.o
