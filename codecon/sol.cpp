#include <algorithm>
#include <vector>
#include <utility>
#include <cstdio>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))

using namespace std;

typedef long long int lli;

int n, k;
int vs[30][10];
int al[30][30];
int al_n[30];
int path_l[30];
int path[30];

int all_less(int* v1, int* v2)
{
    for (int i = 0; i < n; i++) {
        if (v1[i] > v2[i])
            return 0;
    }
    return 1;
}

void search(int v)
{
    int i, ai;
    for (i = 0; i < al_n[v]; i++) {
        ai = al[v][i];
        search();
    }
    for (i = 0; i < al_n[v]; i++) {
        ai = al[v][i];
        if (path_l[ai] < path_l[v]) {
            path_l[v] = path_l[ai];
            path[v] = path[ai];
        }
    }
    path_l[v]++;
}

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;
    while (scanf("%d%d", &k, &n) == 2) {
        for (i = 0; i < k; i++) {
            for (j = 0; j < n; j++) {
                scanf("%d", vs[i] + j);
            }
            sort(vs[i], vs[i] + n);
        }

        for (i = 0; i < k; i++) {
            for (j = 0; j < k; j++) {
                if (all_less(vs[i], vs[j])) {
                    al[i][al_n[i]] = j;
                    al_n[i]++;
                }
            }
        }

        int imax = 0;
        for (i = 1; i < k; i++) {
            if (path_l[i] == 0) {
                search(i);
            }
            if (path_l[i] > max_len) {
                max_len = path_l[i];
            }
        }
        printf("%d\n", max_len);
        for (i = 0; i < path_l[i]; i++) {
            printf("\n");
        }
    }
    return 0;
}

/*
Sample Input
5 2
3 7
8 10
5 2
9 11
21 18
8 6
5 2 20 1 30 10
23 15 7 9 11 3
40 50 34 24 14 4
9 10 11 12 13 14
31 4 18 8 27 17
44 32 13 19 41 19
1 2 3 4 5 6
80 37 47 18 21 9

Sample Output
5
3 1 2 4 5
4
7 2 5 6
*/
