#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

char *s_copy(char *dst, const char *src)
{
    dst[0] = src[0];
    dst[1] = src[1];
    if (!src[1])
        return dst + 1;
    dst[2] = src[2];
    if (!src[2])
        return dst + 2;
    dst[3] = 0;
    return dst + 3;
}

typedef struct {
    char s[8];
    int c;
    int l;
    int bit_c;
    int pad;
} ip2b;

int ip2b_cmp(const void *v1, const void *v2)
{
    ip2b *p1 = v1;
    ip2b *p2 = v2;
    // return strcmp(p1->s, p2->s);
    return *(lli*)p1->s - *(lli*)p2->s;
}

int main(void)
{
    freopen("in", "r", stdin);

    // char qq[8] = {'a','c'}, ww[8] = {'a','b'};
    // printf("=> %d\n", *(lli*)qq < *(lli*)ww);
    // return 0;

    int i, j, k, n = 0, exist[10] = {};
    scanf("%d", &n);
    for (i = 0; i < n; i++) {
        int d;
        scanf("%d", &d);
        exist[d] = 1;
    }

    char str[256][4];
    int um[256];
    j = 0;
    for (i = 0; i < 256; i++) {
        char *s = str[j];
        sprintf(s, "%d", i);
        int v = exist[s[0] - '0'] &&
                s[1] && exist[s[1] - '0'] &&
                s[2] && exist[s[2] - '0'];
        if (v) {
            um[j] = use_mask(s);
            j++;
        }
    }
    int str_c = j;
    printf("str_c %d\n", str_c);

    ip2b bs[65536];
    k = 0;
    for (i = 0; i < str_c; i++) {
        for (j = 0; j < str_c; j++) {
            int bit_c = popc(um[i] | um[j]);
            if (bit_c >= n - 1) {
                *(lli*)bs[k].s = 0;
                bs[k].l = s_copy(s_copy(bs[k].s, str[i]), str[j]) - bs[k].s;
                printf("%s - %d\n", bs[k].s, bs[k].l);
                bs[k].c = 1;
                bs[k].bit_c = bit_c;
                k++;
            }
        }
    }
    printf("--- 1\n");
    printf("k=%d\n", k);

    for (i = 0; i < k; i++) {
        printf("%s\n", bs[i].s);
    }
    printf("qsort\n");
    qsort(bs, k, sizeof *bs, ip2b_cmp);
    for (i = 0; i < k; i++) {
        printf("%s\n", bs[i].s);
    }

    j = 0;
    for (i = 1; i < k; i++) {
        if (ip2b_cmp(&bs[j], &bs[i]) == 0) {
            printf("%s == %s\n", bs[j].s, bs[i].s);
            printf("%d %d\n", j, i);
            bs[j].c++;
        } else {
            j++;
            bs[j] = bs[i];
        }
    }
    int uniques = j + 1;
    printf("--- 2\n");
    printf("uniques %d\n", uniques);

    int c = 0;
    for (i = 0; i < uniques; i++) {
        char b[8];
        ip2b *found;
        if (bs[i].bit_c != n) {
            continue;
        }
        printf("%s c=%d\n", bs[i].s, bs[i].c);

        for (j = 0; j < bs[i].l; j++) {
            b[bs[i].l - 1 - j] = bs[i].s[j];
        }
        b[j] = 0;
        printf("rev %s\n", b);
        found = bsearch(b, bs, k, sizeof *bs, ip2b_cmp);
        if (found) {
            c += bs[i].c * found->c;
        }
        --j;
        b[j] = 0;
        found = bsearch(b, bs, k, sizeof *bs, ip2b_cmp);
        if (found) {
            c += bs[i].c * found->c;
        }
        while (j >= 3 && b[j - 1] == b[j]) {
            --j;
            b[j] = 0;
            found = bsearch(b, bs, k, sizeof *bs, ip2b_cmp);
            if (found) {
                c += bs[i].c * found->c;
            }
        }
    }

    printf("%d\n", c);
    return 0;
}
