#include <algorithm>
#include <vector>
#include <utility>
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int n, k;
int vs[30][10];
int al[30][30];
int al_n[30];
int m[30];
int mark;

int all_less(int* v1, int* v2)
{
    for (int i = 0; i < n; i++) {
        if (v1[i] > v2[i])
            return 0;
    }
    return 1;
}

void dfs(int v, int *p, int *p_n)
{
    int i, q_n = 0, q[30];
    *p_n = 0;
    if (m[v] == mark) {
        return;
    }
    m[v] = mark;
    for (i = 0; i < al_n[v]; i++) {
        dfs(al[v][i], q, &q_n);
        if (q_n > *p_n) {
            *p_n = q_n;
            memcpy(p, q, sizeof q);
        }
    }
    p[*p_n] = v + 1;
    *p_n += 1;
    m[v] = 0;
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    while (scanf("%d%d", &k, &n) == 2) {
        memset(vs, 0, sizeof vs);
        memset(al, 0, sizeof al);
        memset(al_n, 0, sizeof al_n);

        for (i = 0; i < k; i++) {
            for (j = 0; j < n; j++) {
                scanf("%d", vs[i] + j);
            }
            sort(vs[i], vs[i] + n);
        }
        for (i = 0; i < k; i++) {
            for (j = 0; j < k; j++) {
                if (i != j && all_less(vs[j], vs[i])) {
                    al[i][al_n[i]] = j;
                    al_n[i]++;
                }
            }
        }

        int p[30], p_n, pmax[30], pmax_n = 0;
        for (i = 0; i < k; i++) {
            mark++;
            dfs(i, p, &p_n);
            if (p_n > pmax_n) {
                pmax_n = p_n;
                memcpy(pmax, p, sizeof p);
            }
        }

        printf("%d\n", pmax_n);
        for (i = 0; i < pmax_n; i++) {
            printf("%d ", pmax[i]);
        }
        printf("\n");
    }
    return 0;
}
