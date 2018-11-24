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

class Assemble {
public:
    int minCost(const vector<int> &connect) {
        int cost[50][50] = {};
        const int n = connect.size() - 1;
        // By length.
        for (int l = 2; l <= n; l++) {
            // By starting index.
            for (int i = 0; i + l <= n; i++) {
                auto min_cost = INF;
                // By split size.
                for (int k = 1; k < l; k++) {
                    auto x = (connect[i] + k) * (connect[i + l] + l - k) *
                            connect[i + k];
                    auto c = cost[i][i + k - 1] + cost[i + k][i + l - 1] + x;
                    if (c < min_cost) min_cost = c;
                }
                cost[i][i + l - 1] = min_cost;
            }
        }
        return cost[0][n - 1];
    }
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    {
        auto sol = make_unique<Assemble>();
        cout << sol->minCost({19, 50, 10, 39}) << endl;
    }
    {
        auto sol = make_unique<Assemble>();
        cout << sol->minCost({13, 18, 24, 11, 25, 100, 93, 92, 79}) << endl;
    }
    return 0;
}
