#include <stdlib.h>
#include <memory.h>
#include <stdio.h>

FILE *get_input(int argc, char **argv);
void close_input(FILE *fin);

int count(char *t1, char *t2, int n)
{
    return 0;
}

int main(int argc, char **argv)
{
    FILE *fin = get_input(argc, argv);
    close_input(fin);
    return 0;
}

FILE *get_input(int argc, char **argv)
{
    for (int i = 0; i < argc; ++i) {
        if (strcmp(argv[i], "-f") == 0) {
            if (i + 1 < argc) {
                FILE *f = fopen(argv[i + 1], "rt");
                if (f) {
                    return f;
                }
            }
            break;
        }
    }
    return stdin;
}

void close_input(FILE *fin)
{
    if (fin && fin != stdin) {
        fclose(fin);
    }
}
