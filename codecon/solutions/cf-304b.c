/*
   You have been given two dates and your task is to calculate how many days
   are between them.
*/
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n)

typedef long long int lli;

void day_n(int y, int m, int d, int *dn, int *year_days)
{
    int months[12] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};
    if (y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)) {
        *year_days = 366;
        months[1] = 29;
    } else {
        *year_days = 365;
        months[1] = 28;
    }
    int i, c = 0;
    for (i = 1; i < m; i++) {
        c += months[i - 1];
    }
    *dn = c + d;
}

int up(int n, int mod)
{
    int t = n + mod - 1;
    return t - t % mod;
}

int count_div(int x, int y, int mod)
{
    return (up(y, mod) - up(x, mod)) / mod;
}

int day_count(int y1, int m1, int d1, int y2, int m2, int d2)
{
    if (y1 > y2 || (y1 == y2 && (m1 > m2 || (m1 == m2 && d1 > d2)))) {
        int t;
        t = y1;
        y1 = y2;
        y2 = t;
        t = m1;
        m1 = m2;
        m2 = t;
        t = d1;
        d1 = d2;
        d2 = t;
    }
    int dn1, year_days1;
    int dn2, year_days2;
    day_n(y1, m1, d1, &dn1, &year_days1);
    day_n(y2, m2, d2, &dn2, &year_days2);
    if (y1 == y2) {
        return dn2 - dn1;
    }
    int d = (year_days1 - dn1) + dn2;
    y1++;
    if (y1 == y2) {
        return d;
    }
    // y2--;
    int fy = 365 * (y2 - y1);
    int leap_count = count_div(y1, y2, 4) -
                     count_div(y1, y2, 100) +
                     count_div(y1, y2, 400);
    fy += leap_count;
    return d + fy;
}

int main(void)
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int y1, m1, d1;
    int y2, m2, d2;
    scanf("%d:%d:%d", &y1, &m1, &d1);
    scanf("%d:%d:%d", &y2, &m2, &d2);
    printf("%d\n", day_count(y1, m1, d1, y2, m2, d2));
    return 0;
}
