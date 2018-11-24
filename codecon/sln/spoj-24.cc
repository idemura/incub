#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

// Numbers represented as array d[i]: d[0]+10*d[1]+100*d[2]...
// fn is count of digits if the number f[i].
int ln[108][200], ln_n[108];

void printNum(int i) {
    for (int j = ln_n[i]; j--;) {
        printf("%d", ln[i][j]);
    }
    printf("\n");
}

void maxUp(int &n, int k) {
    n = std::max(n, k);
}

void mult(int ii, int k, int io) {
    int *pi = ln[ii], pi_n = ln_n[ii];
    int *po = ln[io], po_n = pi_n;
    for (int i = 0; i < pi_n; i++) {
        int raw_m = pi[i] * k;
        for (int j = i; raw_m != 0; j++) {
            maxUp(po_n, j + 1);
            po[j] += raw_m % 10;
            if (po[j] > 9) {
                maxUp(po_n, j + 2);
                po[j + 1] += po[j] / 10;
                po[j] %= 10;
            }
            raw_m /= 10;
        }
    }
    ln_n[io] = po_n;
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    int test_case[102], max = 0;
    for (int i = 0; i < t; i++) {
        scanf("%d", &test_case[i]);
        if (test_case[i] > max) {
            max = test_case[i];
        }
    }
    ln[1][0] = ln_n[1] = 1;
    for (int i = 2; i <= max; i++) {
        mult(i - 1, i, i);
    }
    for (int i = 0; i < t; i++) {
        printNum(test_case[i]);
    }
    return 0;
}
