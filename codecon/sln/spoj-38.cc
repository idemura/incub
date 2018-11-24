#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

char m[1004][1004];
int mr, mc;

int max4(int *d) {
    int *r = d;
    if (d[1] > *r) r = d + 1;
    if (d[2] > *r) r = d + 2;
    if (d[3] > *r) r = d + 3;
    return r - d;
}

// Here diameter of the tree will be after DFS.
int s_diam = 0;
int dfs(int i, int j) {
    m[i][j] = '!';
    int d[4] = {};
    if (m[i - 1][j] == '.') {
        d[0] = dfs(i - 1, j);
    }
    if (m[i + 1][j] == '.') {
        d[1] = dfs(i + 1, j);
    }
    if (m[i][j - 1] == '.') {
        d[2] = dfs(i, j - 1);
    }
    if (m[i][j + 1] == '.') {
        d[3] = dfs(i, j + 1);
    }
    int maxi = max4(d);
    int max_depth = d[maxi];
    d[maxi] = 0; // Zero to reveal next max value.
    int diam = max_depth + d[max4(d)];
    if (diam > s_diam) {
        s_diam = diam;
    }
    return max_depth + 1;
}

// From problem statement I conclude labyrinth is a tree.
int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    for (; t-- > 0;) {
        scanf("%d%d", &mc, &mr);
        mr++; // Because we have margin of size 1.
        mc++;
        int i0 = -1, j0 = -1;
        for (int i = 1; i < mr; i++) {
            scanf("%s", m[i] + 1);
            m[i][0] = m[i][mc] = '#';
            if (i0 < 0) {
                for (int j = 1; j < mc; j++) {
                    if (m[i][j] == '.') {
                        i0 = i;
                        j0 = j;
                        break;
                    }
                }
            }
        }
        for (int j = 0; j <= mc; j++) {
            m[0][j] = m[mr][j] = '#';
        }

        s_diam = 0;
        dfs(i0, j0);
        printf("Maximum rope length is %d.\n", s_diam);
    }
    return 0;
}
