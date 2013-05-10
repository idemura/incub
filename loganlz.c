#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <regex.h>
#include <ctype.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

typedef long long int lli;

struct page_map
{
    lli addr_lo;
    lli addr_hi;
    char *name;
};

char *scan_word(char *r)
{
    for (; *r && !isspace(*r); ++r) {
    }
    *r++ = 0;
    return r;
}

int parse_str(char *line, struct page_map *pm)
{
    int i;

    char *r = strchr(line, ':');
    if (!r) {
        return 0;
    }
    *r++ = 0;
    for (; isspace(*r); ++r) {
    }
    char *address = r;
    r = scan_word(r);
    sscanf(address, "%llx-%llx", &pm->addr_lo, &pm->addr_hi);
    for (i = 0; i < 4; ++i) {
        r = scan_word(r);
    }
    for (; isspace(*r); ++r) {
    }
    pm->name = r;
    r = scan_word(r);
    return 1;
}

int main(int argc, char **argv)
{
    size_t line_width = 120;
    char *line = malloc(line_width);
    lli sum_vram = 0, sum_total = 0;
    while (getline(&line, &line_width, stdin) != -1) {
        struct page_map pm;
        if (parse_str(line, &pm)) {
            if (strcmp(pm.name, "/dev/mali0") == 0) {
                sum_vram += pm.addr_hi - pm.addr_lo;
            }
            sum_total += pm.addr_hi - pm.addr_lo;
        }
    }
    free(line);

    printf("Total: %lld\n", sum_total);
    printf("VRAM : %lld\n", sum_vram);
    return 0;
}
