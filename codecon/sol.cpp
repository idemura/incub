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
int ml[30];
int nv[30];

int all_less(int* v1, int* v2)
{
    for (int i = 0; i < n; i++) {
        if (v1[i] > v2[i])
            return 0;
    }
    return 1;
}

void dfs(int v)
{
    int i;
    if (ml[v]) {
        return;
    }
    if (al_n[v] == 0) {
        ml[v] = 0;
    } else {
        int vmax = al[v][0];
        for (i = 0; i < al_n[v]; i++) {
            int vi = al[v][i];
            dfs(vi);
            if (ml[vi] > ml[vmax]) {
                vmax = vi;
            }
        }
        ml[v] = ml[vmax] + 1;
        nv[v] = vmax;
    }
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    while (scanf("%d%d", &k, &n) == 2) {
        memset(al_n, 0, sizeof al_n);
        memset(ml, 0, sizeof ml);
        memset(nv, 0, sizeof nv);

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

        int vmax = 0;
        for (i = 0; i < k; i++) {
            dfs(i);
            if (ml[i] > ml[vmax]) {
                vmax = i;
            }
        }

        printf("%d\n", ml[vmax]);
        do {
            printf("%d ", vmax);
            vmax = nv[vmax];
        } while (vmax);
        printf("\n");
        break;
    }
    return 0;
}
