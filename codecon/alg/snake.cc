#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &) = delete;                                                     \
    C &operator=(const C &) = delete;

#define CHECK(E)                                                               \
    do {                                                                       \
        if (!(E)) {                                                            \
            cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
            exit(EXIT_FAILURE);                                                \
        }                                                                      \
    } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

void snake_print(const vector<vector<int>> &m) {
    int i0 = 0, i1 = m.size() - 1;
    int j0 = 0, j1 = m[0].size() - 1;
    for (; i1 > i0 && j1 > j0; i0++, i1--, j0++, j1--) {
        for (int k = j0; k <= j1; k++)
            cout << m[i0][k] << " ";
        for (int k = i0 + 1; k <= i1; k++)
            cout << m[k][j1] << " ";
        for (int k = j1 - 1; k >= j0; k--)
            cout << m[i1][k] << " ";
        for (int k = i1 - 1; k > i0; k--)
            cout << m[k][j0] << " ";
    }
    if (i1 == i0 && j1 >= j0) {
        for (int k = j0; k <= j1; k++)
            cout << m[i0][k] << " ";
    } else if (i1 >= i0 && j1 == j0) {
        for (int k = i0; k <= i1; k++)
            cout << m[k][j0] << " ";
    }
    cout << endl;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    snake_print({{1, 2, 3}, {4, 5, 6}, {7, 8, 9}});
    snake_print({{1, 2}, {4, 5}, {7, 8}});
    snake_print({{1, 2, 3}, {4, 5, 6}});
    snake_print({{1, 2}, {4, 5}});
    return 0;
}
