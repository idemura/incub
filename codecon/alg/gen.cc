#include <algorithm>
#include <map>
#include <stdio.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

#define DIM 3

int generate(int max_value) {
    int c = DIM - 1, count = 0;
    int v[DIM] = {};
    while (true) {
        printf("%d %d %d\n", v[0], v[1], v[2]);
        count++;
        if (v[c] >= max_value) {
            do {
                v[c] = 0;
                c--;
            } while (c >= 0 && v[c] >= max_value);
            if (c < 0) {
                break;
            }
            v[c]++;
            c = DIM - 1;
        } else {
            v[c]++;
        }
    }
    return count;
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int n = generate(3);
    printf("Count %d\n", n);
    return 0;
}
