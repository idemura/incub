#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

void sieve(int n, std::vector<int> *primes) {
    char *seq = new char[n + 1]();
    int sqrtn = (int)sqrt(n);
    int i;
    for (i = 2; i <= sqrtn; i++) {
        if (seq[i]) {
            continue;
        }
        for (int j = i * i; j <= n; j += i) {
            seq[j] = 1;
        }
        primes->push_back(i);
    }
    for (; i <= n; i++) {
        if (!seq[i]) {
            primes->push_back(i);
        }
    }
    delete[] seq;
}

int printPrimesIn(const std::vector<int> &primes, int m, int n) {
    char *seq = new char[n - m + 1]();
    int sqrtn = (int)sqrt(n);
    for (int i = 0; primes[i] <= sqrtn; i++) {
        int p = primes[i];
        int mk = (m / p) * p;
        if (mk < m) mk += p;
        for (; mk <= n; mk += p) {
            seq[mk - m] = mk != p;
        }
    }
    int count = 0;
    for (int i = m; i <= n; i++) {
        if (!seq[i - m] && i != 1) {
            count++;
            printf("%d\n", i);
        }
    }
    printf("\n");
    delete[] seq;
    return count;
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    std::vector<int> primes;
    sieve(32000, &primes);

    int t = 0, n, m;
    scanf("%d", &t);
    for (; t-- > 0;) {
        scanf("%d%d", &m, &n);
        printPrimesIn(primes, m, n);
    }
    return 0;
}
