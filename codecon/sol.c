#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

struct ip {
    char d[8];
    int  d_c;
    int v;
    int mask;
    // int c;
    int from;
    int c;
};

int n, ip_c, mask[10], res_c;
struct ip ip[65536];
unsigned int res[2*65536];

int qsort_ip(const void *v1, const void *v2)
{
    const struct ip *p1 = v1;
    const struct ip *p2 = v2;
    return strcmp(p1->d, p2->d);
}

void digits(int n, char *d, int *d_c)
{
    int i;

    snprintf(d, 8, "%d%d", n >> 8, n & 255);
    *d_c = strlen(d);
    for (i = *d_c; i < 8; i++) {
        d[i] = 0;
    }
}

void revert(char *s, int s_c, char *s_out)
{
    int i;

    for (i = s_c - 1; i >= 0; i--) {
        *s_out++ = s[i];
    }
}

int get_mask(char *d, int d_c)
{
    int i, m = 0;

    for (i = 0; i < d_c; i++) {
        int m_i = mask[d[i] - '0'];
        if (m_i) {
            m |= m_i;
        } else {
            m = 0;
            break;
        }
    }
    return m;
}

int find(char *d)
{
    int i = 0, j = ip_c;
    while (i < j) {
        int m = (i + j) / 2;
        if (strcmp(d, ip[m].d) <= 0) {
            j = m;
        } else {
            i = m + 1;
        }
    }
    return i;
}

int popc(int n)
{
    int c = 0;
    for (; n; n >>= 1) {
        if (n & 1)
            c++;
    }
    return c;
}

int is_start(char *t, char *s)
{
    for (; *t == *s && *t; t++, s++);
    return *t == 0;
}

int is_pal(char *s, int slen)
{
    int i = 0;
    int j = slen - 1;
    for (; i < j && s[i] == s[j]; i++, j--);
    return !(i < j);
}

int count_helper(char *s, struct ip *suffix, int h)
{
    int i = find(s);
    printf("i found it at %d\n", i);
    if (i == ip_c) {
        printf("not found, actually\n");
        return 0;
    }
    if (popc(suffix->mask | ip[i].mask) != n) {
        return 0;
    }
    int c = 0;
    for (; strcmp(s, ip[i].d) == 0; i++) {
        if (h)
            res[res_c++] = (suffix->v << 16) | ip[i].v;
        else
            res[res_c++] = suffix->v | (ip[i].v << 16);
        c++;
    }
    if (!c) {
        printf("nothing...\n");
    } else {
        printf("found c=%d\n", c);
    }
    return c;
}

int count(struct ip *suffix)
{
    printf("count for %s which is %d.%d\n", suffix->d,
            suffix->v >> 8,
            suffix->v & 255);

    int c = 0;
    char rev[8] = {}, revh[8], *r = revh;
    revert(suffix->d, suffix->d_c, rev);
    memcpy(revh, rev, sizeof rev);
    printf("revert is %s\n", rev);
    c += count_helper(rev, suffix, 0);
    // Remove 1.
    int l = suffix->d_c;
    rev[--l] = 0;
    r++;
    printf("now check %s\n", rev);
    c += count_helper(rev, suffix, 0);
    printf("c=%d\n", c);
    printf("now check high %s\n", r);
    c += count_helper(r, suffix, 1);
    printf("c=%d\n", c);
    if (suffix->d[0] == suffix->d[1]) {
        rev[--l] = 0;
        printf("now check %s (11 symm)\n", rev);
        c += count_helper(rev, suffix, 0);
        printf("c=%d\n", c);
    }
    if (suffix->d[suffix->d_c - 1] == suffix->d[suffix->d_c - 2]) {
        r++;
        printf("now check high %s (11 symm)\n", r);
        c += count_helper(r, suffix, 1);
        printf("c=%d\n", c);
    }
    if (suffix->d[0] == suffix->d[2]) {
        rev[--l] = 0;
        printf("now check %s (101 symm)\n", rev);
        c += count_helper(rev, suffix, 0);
        printf("c=%d\n", c);
    }
    if (suffix->d_c > 2 &&
        suffix->d[suffix->d_c - 1] == suffix->d[suffix->d_c - 3]) {
        r++;
        printf("now check high %s (101 symm)\n", r);
        c += count_helper(r, suffix, 1);
        printf("c=%d\n", c);
    }
    return c;
}

int uint_cmp(const void *v1, const void *v2)
{
    return *(unsigned int*)v1 - *(unsigned int*)v2;
}

int main(void)
{
    freopen("in", "r", stdin);

    int i, j;

    scanf("%d", &n);
    for (i = 0; i < n; i++) {
        int d;
        scanf("%d", &d);
        mask[d] = 1 << d;
    }

    j = 0;
    for (i = 0; i < 65536; i++) {
        ZERO(&ip[j], sizeof *ip);
        digits(i, ip[j].d, &ip[j].d_c);
        ip[j].mask = get_mask(ip[j].d, ip[j].d_c);
        if (ip[j].mask) {
            ip[j].v = i;
            ip[j].from = 0;
            ip[j].c = 0;
            j++;
        }
    }
    ip_c = j;

    qsort(ip, ip_c, sizeof *ip, qsort_ip);
    // j = 0;
    // for (i = 1; i < ip_c; i++) {
    //     if (strcmp(ip[j].d, ip[i].d) == 0) {
    //         ip[j].c++;
    //     } else {
    //         j++;
    //         ip[j] = ip[i];
    //     }
    // }
    // ip_c = j;

    for (i = 0; i < ip_c; i++) {
        printf("[%d] %s (%d.%d)\n", i, ip[i].d, ip[i].v >> 8, ip[i].v & 255);
    }

    printf("okay, i have ip_c=%d\n", ip_c);

    int c = 0;
    for (i = 0; i < ip_c; i++) {
        printf("----------------\n");
        ip[i].c = count(&ip[i]);
        c += ip[i].c;
        printf("total %d\n", c);
    }
    printf("res_c %d\n", res_c);

    qsort(res, c, sizeof *res, uint_cmp);
    j = 0;
    for (i = 1; i < res_c; i++) {
        if (res[j] != res[i]) {
            j++;
            res[j] = res[i];
        }
    }
    res_c = j + 1;
    printf("j is %d\n", j + 1);

    for (i = 0; i < j; i++) {
        printf("%d.%d.%d.%d\n",
            (res[i] >> 24),
            (res[i] >> 16) & 255,
            (res[i] >> 8) & 255,
            res[i] & 255);
    }
    return 0;
}
