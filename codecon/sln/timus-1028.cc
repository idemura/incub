#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <functional>
#include <iostream>
#include <limits.h>
#include <map>
#include <math.h>
#include <queue>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define INF 0x7fffffff
#define NON_COPYABLE(C)                                                        \
    C(const C &);                                                              \
    C &operator=(const C &);

using namespace std;

typedef long long int i64;

// Must be power of 2.
static const int kMax = 1 << 15;
static const int kMaxBit = kMax / 2;

int getLeftCount(const vector<int> &tree, int i) {
    int l = 2 * i + 1;
    if (l < tree.size())
        return tree[l];
    else
        return 0;
}

int countLeftRec(const vector<int> &tree, int x, int i, int bit) {
    int res = 0;
    if (i < tree.size()) {
        if (x & bit) {
            res = countLeftRec(tree, x, 2 * i + 2, bit >> 1) +
                    getLeftCount(tree, i);
        } else {
            res = countLeftRec(tree, x, 2 * i + 1, bit >> 1);
            if (bit == 0) {
                // Add how much in the leaf, because it matches to x.
                // May skip recursion too.
                res += tree[i];
            }
        }
    }
    return res;
}

int countLeft(const vector<int> &tree, int x) {
    return countLeftRec(tree, x, 0, kMaxBit);
}

void insertRec(vector<int> &tree, int x, int i, int bit) {
    if (i < tree.size()) {
        tree[i] += 1;
        if (x & bit) {
            insertRec(tree, x, 2 * i + 2, bit >> 1);
        } else {
            insertRec(tree, x, 2 * i + 1, bit >> 1);
        }
    }
}

void insert(vector<int> &tree, int x) {
    insertRec(tree, x, 0, kMaxBit);
}

int main(int argc, char **argv) {
    int n;
    scanf("%d", &n);
    vector<int> tree(kMax * 2 - 1);
    vector<int> levels(n);
    // Input is stored increasing y. Same y-s by increasing x.
    for (int i = 0; i < n; i++) {
        int x, y;
        scanf("%d%d", &x, &y);
        int rank = countLeft(tree, x);
        levels[rank]++;
        insert(tree, x);
    }
    for (auto l : levels) {
        printf("%d\n", l);
    }
    return 0;
}
