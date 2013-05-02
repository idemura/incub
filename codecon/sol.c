#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

/* MOD is prime */
#define MOD 1000000007

typedef long long int lli;

int a, b, n;
int *cnk_table;

int madd(int a, int b)
{
    return (int)( ((lli)a + (lli)b) % MOD );
}

int mmul(int a, int b)
{
    return (int)( ((lli)a * (lli)b) % MOD );
}

int mrev(int a)
{
    /* Performs Euler's algorithm for find Bezout's coefficients. Assume `a` is
       less than `b`. It's true for this task.
    */
    int b = MOD;
    int x = 0, y = 1;
    int u = 1, v = 0;
    while (a != 0) {
        div_t qr = div(b, a);
        int m = x - u * qr.quot;
        int n = y - v * qr.quot;
        b = a;
        a = qr.rem;
        x = u, y = v;
        u = m, v = n;
    }
    if (x < 0) {
        x += MOD;
    }
    return x;
}

/* Gets nth digit of `n` in decimal form. Digits counted from the rightmost,
   least significant, which is 0
*/
int nth_digit(int j, int n)
{
    while (n != 0) {
        int d = n % 10;
        n /= 10;
        if (j == 0) {
            return d;
        }
        j--;
    }
    return 0;
}

int get_num(int k)
{
    return b * k + (n - k) * a;
}

int digit_lower_bound(int d, int pos, int lo, int hi)
{
    while (lo < hi) {
        int mid = (lo + hi) / 2;
        int nmid = get_num(mid);
        int dmid = nth_digit(pos, nmid);
        if (d > dmid) {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    return lo;
}

int cnk(int n, int k)
{
    // printf("cnk n=%d k=%d\n", n, k);
    /* Dynamic programming is here. */
    if (k == 0 && n >= 0) {
        return 1;
    }
    if (n == 0) {
        return 0;
    }
    return madd(cnk(n - 1, k), cnk(n - 1, k - 1));
}

int *create_cnk_table(int n)
{
    int *t = malloc((n + 1) * sizeof *t);
    int i;
    t[0] = 1;
    for (i = 1; i <= n; ++i) {
        int r = mrev(i);
        t[i] = mmul(mmul(t[i - 1], n - i), r);
        printf("%d ", t[i]);
    }
    printf("\n");
    return t;
}

int solve(int pos, int lo, int hi);

int solve_digit(int d, int pos, int lo, int hi)
{
    printf("solve for digit %d\n", d);
    int d_lo = digit_lower_bound(d, pos, lo, hi);
    printf("d_lo %d -> %d\n", d_lo, get_num(d_lo));
    if (nth_digit(pos, get_num(d_lo)) == d) {
        int d_hi = digit_lower_bound(d + 1, pos, lo, hi);
        return solve(pos - 1, d_lo, d_hi);
    }
    return 0;
}

int solve(int pos, int lo, int hi)
{
    if (lo == hi) {
        printf("lo == hi: %d %d\n", lo, hi);
        return 0;
    }
    if (pos < 0) {
        printf("rec leaf, lo=%d hi=%d return %d\n", lo, hi, hi - lo);
        // int count = hi - lo;
        printf("sums of long numbers:");
        int count = 0;
        for (int i = lo; i < hi; ++i) {
            printf("cnk of n=%d i=%d\n", n, i);
            int ci = cnk(n, i);
            count = madd(count, ci);
            printf(" %d", get_num(i));
        }
        printf("\n");
        return count;
    }

    int a_res = solve_digit(a, pos, lo, hi);
    int b_res = solve_digit(b, pos, lo, hi);
    return madd(a_res, b_res);
}

int high_digit_pos(int n)
{
    int l = 6;
    for(; nth_digit(l, n) == 0; l--) {
    }
    return l;
}

int pow10(int n)
{
    int p = 1;
    for(; n != 0; n--) {
        p *= 10;
    }
    return p;
}

int main(int argc, char **argv)
{
    int combinations = 0;
/*
#ifndef ONLINE_JUDGE
     freopen("in", "r", stdin);
 #endif
*/
    scanf("%d%d%d", &a, &b, &n);
    if (a > b) {
        int temp = a;
        a = b;
        b = temp;
    }

    cnk_table = create_cnk_table(4);
    return 0;

    /* Simplify a bit by reducing leading digits that are zeroes */
    int min_sum = a * n;
    int max_sum = b * n;
    printf("min_sum %d max_sum %d\n", a * n, max_sum);
    int lmin = high_digit_pos(min_sum);
    int lmax = high_digit_pos(max_sum);
    printf("lmin %d lmax %d\n", lmin, lmax);
    if (lmin != lmax) {
        int c1, c2;
        div_t qr = div(pow10(lmax) - a * n, b - a);
        int k = qr.rem ? qr.quot + 1 : qr.quot;
        printf("%d %d %d\n", get_num(k - 1), get_num(k), get_num(k + 1));
        c1 = solve(lmin, 0, k);
        c2 = solve(lmax, k, n + 1);
        combinations = madd(c1, c2);
    } else {
        printf("starting from pos %d\n", lmin);
        combinations = solve(lmin, 0, n + 1);
    }

    free(cnk_table);
    printf("%d\n", combinations);
    return 0;
}
