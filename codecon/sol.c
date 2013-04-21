#include <stdlib.h>
#include <memory.h>
#include <stdio.h>
#include <assert.h>

FILE *get_input(int argc, char **argv);
void close_input(FILE *fin);

int main(int argc, char **argv)
{
    return 0;
}

FILE *get_input(int argc, char **argv)
{
    int i; /* C89 ready for codeforces.com */
    for (i = 0; i < argc; ++i) {
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
