#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

// char *s_copy(char *dst, const char *src)
// {
//     dst[0] = src[0];
//     dst[1] = src[1];
//     if (!src[1])
//         return dst + 1;
//     dst[2] = src[2];
//     if (!src[2])
//         return dst + 2;
//     dst[3] = 0;
//     return dst + 3;
// }

struct ip {
    char d[8];
    int  d_c;
    int val;
    int mask;
    int pad;
};

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

void revert(char *ds, int ds_c, char *ds_out)
{
    int i;

    for (i = ds_c - 1; i >= 0; i--) {
        *ds_out++ = ds[i];
    }
}

int get_mask(int *mask, char *d, int d_c)
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

int find(struct ip *ip, int i, int j, char *d)
{
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

int count(struct ip *ip, int ip_c, char *d)
{
    int j;
    int i = find(ip, 0, ip_c, d);
    if (i == ip_c) {
        return 0;
    }
    if (!ip[i].mask) {
        return 0;
    }
    int c = 1;
    for (j = i + 1; j < ip_c && strcmp(ip[i].d, ip[j].d) == 0; j++) {
        c++;
    }
    return c;
}

int main(void)
{
    freopen("in", "r", stdin);

    int i, j, n, mask[10] = {};
    struct ip ip[65536];

    scanf("%d", &n);
    for (i = 0; i < n; i++) {
        int d;
        scanf("%d", &d);
        mask[d] = 1 << d;
    }

    j = 0;
    for (i = 0; i < 65536; i++) {
        digits(i, ip[j].d, &ip[j].d_c);
        ip[j].mask = get_mask(mask, ip[j].d, ip[j].d_c);
        if (ip[j].mask) {
            ip[j].val = i;
            j++;
        }
    }
    int ip_c = j;

    qsort(ip, ip_c, sizeof *ip, qsort_ip);

    int c = 0;
    for (i = 0; i < ip_c; i++) {
        if (popc(ip[i].mask) != n) {
            continue;
        }
        char r[8] = {};
        int l = ip[i].d_c;
        revert(ip[i].d, l, r);
        c += count(ip, ip_c, r);
        l--;
        ip[i].d[l] = 0;
        c += count(ip, ip_c, r);
    }

    printf("%d\n", c);
    return 0;
}
