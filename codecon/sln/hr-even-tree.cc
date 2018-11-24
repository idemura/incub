#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <random>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define fori(N) for (int i = 0; i < N; i++)
#define forj(N) for (int j = 0; j < N; j++)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;
constexpr int DIM = 102;

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int n, m;
    cin >> n >> m;
    int a[DIM][DIM] = {};
    int d[DIM] = {};
    int c[DIM] = {};
    int removed[DIM] = {};
    int n_rem = 0;
    for (int i = 0; i < m; i++) {
        int v, w;
        cin >> v >> w;
        a[v][w] = a[w][v] = 1;
        d[v]++;
        d[w]++;
    }
    for (int i = 1; i <= n; i++)
        c[i] = 1;
    int res = 0;
    while (n_rem < n) {
        for (int i = 1; i <= n; i++) {
            if (d[i] <= 1 && !removed[i]) {
                if (c[i] % 2 == 0) {
                    res++;
                } else {
                    for (int j = 1; j <= n; j++) {
                        if (!removed[j] && a[i][j]) c[j] += c[i];
                    }
                }
                for (int j = 1; j <= n; j++) {
                    if (!removed[j] && a[i][j]) d[j]--;
                }
                removed[i] = 1;
                n_rem++;
            }
        }
    }
    cout << (res - 1) << endl;
    return 0;
}
