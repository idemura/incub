#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

#define MOD 1000000007

typedef long long int lli;

int a, b, n;

int madd(int a, int b)
{
    return (int)( ((lli)a + (lli)b) % MOD );
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

int Cnk(int k, int n)
{
    /* Dynamic programming is here. */
    if (k == 0 && n >= 0) {
        return 1;
    }
    if (n == 0) {
        return 0;
    }
    return madd(Cnk(k, n - 1), Cnk(k - 1, n - 1));
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
            int ck = Cnk(i, n);
            count = madd(count, ck);
            printf(" %d", get_num(i));
        }
        printf("\n");
        return count;
    }

    int a_res = solve_digit(a, pos, lo, hi);
    int b_res = solve_digit(b, pos, lo, hi);
    return madd(a_res, b_res);
}

int main(int argc, char **argv)
{
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

    /* Simplify a bit by reducing leading digits that are zeroes */
    int max_sum = b * n;
    int pos = 6;
    while (nth_digit(pos, max_sum) == 0) {
        pos--;
    }

    printf("starting from pos %d\n", pos);
    int combinations = solve(pos, 0, n + 1);
    printf("%d\n", combinations);
    return 0;
}
