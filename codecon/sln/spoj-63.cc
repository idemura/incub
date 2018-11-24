#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <limits.h>
#include <map>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C)                                                        \
    C(const C&);                                                               \
    C& operator=(const C&);

using namespace std;

typedef long long int lli;

struct MemCell {
    int ver, val;
};

int n, k, ver, patt[42];
MemCell mem[38][38][19];

int backtrack(int i, int s, int o) {
    for (;;) {
        if (o < 0 || o > s) {
            return 0;
        }
        if (mem[i][s][o].ver == ver) {
            return mem[i][s][o].val;
        }
        if (o == s) {
            for (int j = i; j < 2 * n; j++) {
                if (patt[j] == '[') {
                    o++;
                } else {
                    o--;
                    s--;
                }
            }
            mem[i][s][o].ver = ver;
            mem[i][s][o].val = o == 0;
            return mem[i][s][o].val;
        }
        if (patt[i] == '[') {
            // Space count doesn't decrease.
            i++;
            o++;
        } else {
            mem[i][s][o].ver = ver;
            mem[i][s][o].val = backtrack(i + 1, s - 1, o + 1) +
                    backtrack(i + 1, s - 1, o - 1);
            return mem[i][s][o].val;
        }
    }
}

int countBracketExprWithRestr() {
    return backtrack(0, 2 * n - k, 0);
}

int main(int argc, char** argv) {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif

    int test_num = 0;
    scanf("%d", &test_num);
    for (ver = 1; ver <= test_num; ver++) {
        scanf("%d%d", &n, &k);
        int sk[40] = {};
        for (int j = 0; j < k; j++) {
            scanf("%d", &sk[j]);
        }
        memset(patt, 0, sizeof patt);
        for (int j = 0; j < k; j++) {
            patt[sk[j] - 1] = '[';
        }
        printf("%d\n", countBracketExprWithRestr());
    }
    return 0;
}
