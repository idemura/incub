#define _GNU_SOURCE
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <search.h>
#include <stdio.h>

#define array_of(a) (sizeof(a) / sizeof(a[0]))

typedef void *rbtree_t;
typedef long long int lli;

int str_cmpf(const void *p1, const void *p2) {
    return strcmp((const char*)p1, (const char*)p2);
}
int int_cmpf(const void *p1, const void *p2) {
    return (long)p1 - (long)p2;
}
void no_free(void *p) {
}

int main(int argc, char **argv)
{
    int t = 0, sx, sy, ex, ey;
    int i, nx, ny;
    char *wind = 0;
    char chr_x, chr_y;

    scanf("%d %d %d %d %d", &t, &sx, &sy, &ex, &ey);
    wind = malloc((t + 1) * sizeof(*wind));
    scanf(" %s", wind);

    chr_x = ex > sx? 'E': 'W';
    nx = abs(ex - sx);
    chr_y = ey > sy? 'N': 'S';
    ny = abs(ey - sy);
    for (i = 0; i < t; ++i) {
        if (wind[i] == chr_x && nx > 0)
            nx--;
        if (wind[i] == chr_y && ny > 0)
            ny--;
        if (nx == 0 && ny == 0) {
            break;
        }
    }
    free(wind);
    printf("%d\n", i < t? i + 1: -1);
    return 0;
}
