#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n);

typedef long long int lli;

int primes[1000];
int primes_n;

int is_prime(int n)
{
    int i;
    for (i = 2; i < n; i++) {
        if (n % i == 0)
            return 0;
    }
    return 1;
}

int check(int n)
{
    int s = 0;
    int i;
    for (i = 2; i < n; i++) {
        if (is_prime(i)) {
            printf("%d ", i);
            s++;
        }
    }
    printf("\n");
    return s;
}

/* Takes `list` with size `list_n` as a buffer (no assumptions on values in it)
   and runs Eratosthenes sieve on it. Returns count of primes in list, which
   stored in 0..list_n in the list. Max prime can be found is list_n + 1.

   Example of usage:

    int primes[100];
    int primes_n = sieve(primes, ARRAY_SIZEOF(primes));
*/
int sieve(int *list, int list_n)
{
    ZERO(list, list_n * sizeof *list);
    int i, j, w = 0;
    int sqrt_n = (int)sqrt(list_n);
    for (i = 0; i <= sqrt_n; ++i) {
        if (!list[i]) {
            int prime = i + 2;
            /* The first index to go, -2 for index shift (don't 0 and 1 take
               into account).
               Start from square of prime p. Let's assume the opposite: there
               is some composite m = p * j, m < p * p, hence j < p.  Therefore,
               this j should have some prime divisor p' <= j < p and hence,
               m should be rejected in the process for j.
            */
            int first = prime * prime - 2;
            for (j = first; j < list_n; j += prime) {
                list[j] = 1;
            }
        }
    }
    // Copy primes to the beginning.
    for (i = 0; i < list_n; i++) {
        if (!list[i]) {
            list[w++] = i + 2;
        }
    }
    return w;
}

int main(int argc, char **argv)
{
    // freopen("in", "r", stdin);
    int i, k;

    for (k = 2; k < 1000; k++) {
        // primes_n = sieve(primes, ARRAY_SIZEOF(primes));
        primes_n = sieve(primes, k);
        for (i = 0; i < primes_n; i++) {
            printf("%d ", primes[i]);
        }
        printf("\n%d\n", primes_n);
        printf("check:\n");
        int cc;
        printf("%d\n", cc=check(k + 2));
        assert(cc == primes_n);
    }
    return 0;
}
