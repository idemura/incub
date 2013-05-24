#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

#define MOD 20

int factor[MOD + 1];
int prime[MOD];
int prime_n;

int sieve(int *list, int list_n)
{
    ZERO(list, list_n * sizeof *list);
    int i, j, w = 0;
    int sqrt_n = (int)sqrt(list_n);
    for (i = 0; i <= sqrt_n; ++i) {
        if (!list[i]) {
            int prime = i + 2;
            int first = prime * prime - 2;
            for (j = first; j < list_n; j += prime) {
                list[j] = 1;
                factor[j + 1] = prime;
            }
            factor[i + 1] = prime;
        }
    }
    // Copy primes to the beginning.
    for (i = 0; i < list_n; i++) {
        if (!list[i]) {
            list[w++] = i + 2;
            factor[i + 1] = i + 2;
        }
    }
    return w;
}

void factorize(int n)
{
    printf("input factorize %d\n", n);
    int i = n;
    for (; i != 1;) {
        int f = factor[i - 1];
        printf("factor %d\n", f);
        i /= f;
    }
}

void factorize_factorial(int n)
{
    int fc[MOD + 1] = {};
    int i;
    printf("input factorize factorial %d\n", n);
    fc[n - 1] = 1;
    for (i = n; i != 1; i--) {
        int f = factor[i - 1];
        int c = fc[i - 1];
        printf("%d: factor %d and count %d\n", i, f, c);
        if (f != i) {
            fc[f - 1] += c;
            fc[i / f - 1] += c;
        } else {
            // So, i is a prime
            printf("%d ^ %d\n", f, fc[i - 1] + 1);
            fc[f - 1] += 1;
        }
    }
    printf("end\n");
}

int main(void)
{
    // int i;
    prime_n = sieve(prime, MOD);
    // for (i = 0; i < MOD; i++) {
    //     printf("%d -> %d\n", i + 1, factor[i]);
    // }
    // factorize(14);
    factorize_factorial(5);
    return 0;
}
