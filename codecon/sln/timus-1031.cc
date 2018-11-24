#include <algorithm>
#include <map>
#include <stdio.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

void read3Ints(int *is) {
    scanf("%d%d%d", is, is + 1, is + 2);
}

int xs[10001];
int vs[10001];
int ls[3], cs[3];
int n, b, e;

int update(int p, int start, int l, int c) {
    int i;

    if (start == p) {
        start++;
    }
    int new_c = vs[p] + c;
    int new_x = xs[p] + l;
    for (i = start; i < n && xs[i] <= new_x; i++) {
        if (vs[i] == 0 || new_c < vs[i]) {
            vs[i] = new_c;
        }
    }
    return i;
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j;

    read3Ints(ls);
    read3Ints(cs);
    scanf("%d%d%d", &n, &b, &e);
    for (i = 1; i < n; i++) {
        scanf("%d", xs + i);
    }
    // 1-based to 0-based.
    b--;
    e--;
    if (b > e) {
        int t = b;
        b = e;
        e = t;
    }
    int start[3] = {b + 1, b + 1, b + 1};
    for (i = b; i < e; i++) {
        for (j = 0; j < 3; j++) {
            start[j] = update(i, start[j], ls[j], cs[j]);
        }
    }
    printf("%d\n", vs[e]);
    return 0;
}
