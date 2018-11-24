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

int B[200][200];
int D[200][200];
int n, m;

struct I2d {
    int i, j;

    I2d(): i(), j() {}
    I2d(int i, int j): i(i), j(j) {}
};

// `b` is border, all bitmap pixels with distance 0, 1, 2... on each step of
// algorithm. Initialized to 0 distances on enter.
void fillNearestDistance(std::vector<I2d> *b) {
    // Since we have no more 200*200=40000 cells, we'll be always inserting into
    // `b`, but not clearing it.
    int bsz0 = 0, bsz = b->size();
    for (; bsz0 < bsz;) {
        for (int i = bsz0; i < bsz; i++) {
            int ii = (*b)[i].i;
            int jj = (*b)[i].j;
            int new_d = D[ii][jj] + 1;
            if (ii > 0 && new_d < D[ii - 1][jj]) {
                D[ii - 1][jj] = new_d;
                b->push_back(I2d(ii - 1, jj));
            }
            if (ii + 1 < n && new_d < D[ii + 1][jj]) {
                D[ii + 1][jj] = new_d;
                b->push_back(I2d(ii + 1, jj));
            }
            if (jj > 0 && new_d < D[ii][jj - 1]) {
                D[ii][jj - 1] = new_d;
                b->push_back(I2d(ii, jj - 1));
            }
            if (jj + 1 < m && new_d < D[ii][jj + 1]) {
                D[ii][jj + 1] = new_d;
                b->push_back(I2d(ii, jj + 1));
            }
        }
        bsz0 = bsz;
        bsz = b->size();
    }
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    for (; t-- > 0;) {
        scanf("%d%d", &n, &m);
        char str[200] = {};
        std::vector<I2d> b;
        for (int i = 0; i < n; i++) {
            scanf("%s", str);
            for (int j = 0; str[j]; j++) {
                B[i][j] = str[j] == '1';
                D[i][j] = B[i][j] ? 0 : INF;
                if (B[i][j]) {
                    b.push_back(I2d(i, j));
                }
            }
        }
        fillNearestDistance(&b);
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                printf("%d ", D[i][j]);
            }
            printf("\n");
        }
    }
    return 0;
}
