#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

#define MOD 20

int divisor[MOD + 1];
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
                divisor[j + 1] = prime;
            }
            divisor[i + 1] = prime;
        }
    }
    // Copy primes to the beginning.
    for (i = 0; i < list_n; i++) {
        if (!list[i]) {
            list[w++] = i + 2;
            divisor[i + 1] = i + 2;
        }
    }
    return w;
}

void factorize(int n)
{
    printf("input factorize %d\n", n);
    int i = n;
    for (; i != 1;) {
        int f = divisor[i - 1];
        printf("divisor %d\n", f);
        i /= f;
    }
}

// Factorize sum of (i + 1)!^fp[i], i = 0..n-1.
void factorize_factorial(int *fp, int n)
{
    // int fc[MOD + 1] = {};
    int i;
    for (i = n; i-- > 1;) {
        fp[i - 1] += fp[i];
    }
    for (i = 0; i < fp[i]; i++) {
        printf("%d ", fp[i]);
    }
    printf("\n");

    // printf("input factorize factorial %d\n", n);
    // fc[n - 1] = 1;
    for (i = n; i != 1; i--) {
        int f = divisor[i - 1];
        // int c = fc[i - 1] += 1;
        printf("%d: divisor %d and count %d\n", i, f, fp[i - 1]);
        if (f != i) {
            int c = fp[i - 1];
            fp[f - 1] += c;
            fp[i / f - 1] += c;
            printf("Updated: %d and %d to %d and %d\n", f, i / f, fp[f - 1], fp[i /f - 1]);
            fp[i - 1] = 0; // Only for print in the end.
        } else {
            // So, i is a prime
            printf("prime %d ^ %d\n", f, fp[i - 1]);
            // fc[f - 1] += c;
        }
    }
    // Put 1 power to zero.
    fp[0] = 0;
    printf("--------\n");
    for (i = 0; i < n; i++) {
        if (fp[i]) {
            printf("%d ^ %d\n", i + 1, fp[i]);
        }
    }
    printf("end\n");
}

int main(void)
{
    // int i;
    // for (i = 0; i < 26; i++) {
    //     printf("%d -> %d\n", i, sqrti(i));
    // }
    prime_n = sieve(prime, MOD);
    // for (i = 0; i < MOD; i++) {
    //     printf("%d -> %d\n", i + 1, divisor[i]);
    // }
    // factorize(14);
    int fp[8] = {};
    fp[7] = 2;
    fp[2] = 1;
    factorize_factorial(fp, ARRAY_SIZEOF(fp));
    return 0;
}
