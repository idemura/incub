#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

const int DIM = 26;
int mat[DIM][DIM];
int visited[DIM];

void clear() {
    for (int i = 0; i < DIM; i++) {
        visited[i] = 0;
        for (int j = 0; j < DIM; j++) {
            mat[i][j] = 0;
        }
    }
}

// Get connected component vertex count.
int connectedComponent(int v, int depth) {
    if (visited[v]) {
        return 0;
    }
    visited[v] = 1;
    int cc_num = 1;
    for (int i = 0; i < DIM; i++) {
        if (mat[v][i]) {
            cc_num += connectedComponent(i, depth + 1);
        }
    }
    return cc_num;
}

void solve() {
    static const char kPossible[] = "Ordering is possible.";
    static const char kImpossible[] = "The door cannot be opened.";

    clear();
    int n;
    scanf("%d", &n);
    char buf[1024];
    // `first` is some vertex in the indirected graph. We will check if whole
    // graph is a connected component.
    int first = -1, alpha_num = 0, alpha[DIM] = {};
    int counter[DIM] = {};
    for (int i = 0; i < n; i++) {
        scanf("%s", buf);
        int i0 = buf[0] - 'a';
        int i1 = buf[strlen(buf) - 1] - 'a';
        if (!alpha[i0]) {
            alpha[i0] = 1;
            alpha_num++;
        }
        if (!alpha[i1]) {
            alpha[i1] = 1;
            alpha_num++;
        }
        counter[i0]++;
        counter[i1]--;
        // Add edge into adjacency matrix of indirected graph:
        mat[i0][i1] = mat[i1][i0] = 1;
        if (first < 0) {
            // The vertex we will start connected components search.
            first = i0;
        }
    }
    int n0 = 0, n1 = 0, others = 0;
    for (int i = 0; i < 26; i++) {
        if (counter[i] == -1) {
            n0++;
        } else if (counter[i] == 1) {
            n1++;
        } else if (counter[i] != 0) {
            others++;
            break; // Others have to be 0, can break now.
        }
    }
    if (n0 <= 1 && n1 <= 1 && others == 0) {
        // We could have inaccessible components of the graph. Check the graph
        // is connected.
        int cc_size = connectedComponent(first, 0);
        if (cc_size == alpha_num) {
            printf("%s\n", kPossible);
        } else {
            printf("%s\n", kImpossible);
        }
    } else {
        printf("%s\n", kImpossible);
    }
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    int t = 0;
    scanf("%d", &t);
    for (; t-- > 0;) {
        solve();
    }
    return 0;
}
