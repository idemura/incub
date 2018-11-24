// Prufer code.
#include <algorithm>
#include <math.h>
#include <set>
#include <stdio.h>
#include <string.h>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define DIM 7500

int vs[DIM], vs_n = 0;
int cs[DIM], cs_n = 0;
vector<int> al[DIM];

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    set<int> heap; // Seems set is good enough to be a heap.
    int v, i, j;

    while (scanf("%d", &v) == 1) {
        vs[vs_n++] = v - 1;
        if (v > cs_n) {
            cs_n = v;
        }
    }
    for (i = 0; i < vs_n; i++) {
        cs[vs[i]]++;
    }
    for (i = 0; i < cs_n; i++) {
        if (cs[i] == 0) {
            heap.insert(i);
        }
    }
    for (i = 0; i < vs_n; i++) {
        int v = *heap.begin();
        heap.erase(heap.begin());
        int w = vs[i];
        al[v].push_back(w);
        al[w].push_back(v);
        cs[w]--;
        if (cs[w] == 0) {
            heap.insert(w);
        }
    }
    for (i = 0; i < cs_n; i++) {
        sort(al[i].begin(), al[i].end());
        printf("%d:", i + 1);
        for (j = 0; j < al[i].size(); j++) {
            printf(" %d", al[i][j] + 1);
        }
        printf("\n");
    }
    return 0;
}
