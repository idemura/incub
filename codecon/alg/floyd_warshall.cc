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
#define INF 0x3fffffff

typedef long long int lli;

void floydWarshall(int **am, int **pm, int n) {
    // Matrix element `am[i][j]` is the shortest path length from `i` to `j`.
    for (int k = 0; k < n; k++) {
        // For every pair of vertices `i` and `j` check if shorter path goes
        // through `k`: i->k->k->j:
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (am[i][k] + am[k][j] < am[i][j]) {
                    am[i][j] = am[i][k] + am[k][j];
                    pm[i][j] = k;
                }
            }
        }
    }
}

void pathRec(int **pm, int u, int v, std::vector<int> *path) {
    int k = pm[u][v];
    if (k == v) {
        path->push_back(v);
    } else {
        pathRec(pm, u, k, path);
        pathRec(pm, k, v, path);
    }
}

// Input (output of Floyd-Warshall algorithm):
//  pm - shortest paths matrix.
//  u and v - vertices to find the shortest path between.
// Output:
//  p - path, including u and v.
void getShortestPath(int **pm, int u, int v, std::vector<int> *path) {
    if (u == v) {
        path->push_back(u);
    } else if (pm[u][v] >= 0) {
        path->push_back(u);
        pathRec(pm, u, v, path);
    }
}

int **newIntMat(int n, int fill, int fill_diag) {
    int **m = new int *[n]();
    m[0] = new int[n * n]();
    for (int i = 1; i < n; i++) {
        m[i] = m[i - 1] + n;
    }
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            m[i][j] = fill;
        }
        m[i][i] = fill_diag;
    }
    return m;
}

void deleteIntMat(int **m) {
    if (m) {
        delete[] m[0];
        delete[] m;
    }
}

void addEdge(int **am, int **pm, int i, int j, int len) {
    am[i][j] = len;
    pm[i][j] = j;
}

void printMat(int **m, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            if (m[i][j] == INF) {
                printf("+I ");
            } else {
                printf("%2d ", m[i][j]);
            }
        }
        printf("\n");
    }
}

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    const int n = 4;
    int **am = newIntMat(n, INF, 0), **pm = newIntMat(n, -1, -1);
    // for (int i = 0; i < n; i++) {
    //   pm[i][i] = i;
    // }
    addEdge(am, pm, 0, 1, 5);
    addEdge(am, pm, 0, 3, 10);
    addEdge(am, pm, 2, 3, 1);
    addEdge(am, pm, 1, 2, 3);
    printf("am:\n");
    printMat(am, n);
    printf("pm:\n");
    printMat(pm, n);

    floydWarshall(am, pm, n);
    printf("Floyd-Warshall:\n");
    printf("am:\n");
    printMat(am, n);
    printf("pm:\n");
    printMat(pm, n);
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            std::vector<int> sp;
            getShortestPath(pm, i, j, &sp);
            if (am[i][j] == INF) {
                printf("No path %d to %d\n", i, j);
            } else {
                printf("Shortest path %d to %d is %d\n", i, j, am[i][j]);
                printf("  ");
                for (int k = 0; k < sp.size(); k++) {
                    printf("%d ", sp[k]);
                }
                printf("\n");
            }
        }
    }
    return 0;
}
