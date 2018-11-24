#include <algorithm>
#include <stdio.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

typedef long long int lli;

int M, N;
int C[100][500];
int T[101][500];
int m1[500];
int m2[500];
int path[500 * 500];

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, k;

    scanf("%d%d", &M, &N);
    for (i = 0; i < M; i++) {
        for (j = 0; j < N; j++) {
            scanf("%d", &C[i][j]);
            // C[i][j]++;
            T[i + 1][j] = INF;
        }
    }

    for (i = 1; i <= M; i++) {
        j = 0;
        m1[j] = T[i - 1][j] + C[i - 1][j];
        for (j++; j < N; j++) {
            m1[j] = min(m1[j - 1], T[i - 1][j]) + C[i - 1][j];
        }
        j = N - 1;
        m2[j] = T[i - 1][j] + C[i - 1][j];
        for (j--; j >= 0; j--) {
            m2[j] = min(m2[j + 1], T[i - 1][j]) + C[i - 1][j];
        }
        for (j = 0; j < N; j++) {
            T[i][j] = min(m1[j], m2[j]);
        }
    }

    int jmin = 0;
    for (j = 1; j < N; j++) {
        if (T[M][j] < T[M][jmin]) {
            jmin = j;
        }
    }
    i = M;
    j = jmin;
    k = 0;
    do {
        path[k++] = j + 1;
        int m = T[i - 1][j];
        jmin = j;
        if (j - 1 >= 0 && T[i][j - 1] < m) {
            jmin = j - 1;
            m = T[i][jmin];
        }
        if (j + 1 < N && T[i][j + 1] < m) {
            jmin = j + 1;
            m = T[i][jmin];
        }
        if (m == T[i - 1][j])
            i--;
        else
            j = jmin;
    } while (i);

    for (i = 0; i < k; i++) {
        printf("%d ", path[k - 1 - i]);
    }
    printf("\n");

    return 0;
}
