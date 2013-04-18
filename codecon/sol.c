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
    int n;
    fscanf(fin, " %d", &n);
    char *t1 = malloc(n + 1);
    char *t2 = malloc(n + 1);
    fscanf(fin, " %s", t1);
    fscanf(fin, " %s", t2);
    count(t1, t2, n);
    free(t1);
    free(t2);
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
