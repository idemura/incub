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
    C(const C&) = delete;                                                      \
    C& operator=(const C&) = delete;

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

struct IntIx {
    int v = 0;
    int i = 0;
    IntIx(int v, int i): v(v), i(i) {}
};

class ChangeOptimizer {
public:
    vector<int> fewestCoins(vector<int> coin_types, int value) {
        vector<IntIx> temp;
        for (int i = 0; i < coin_types.size(); i++) {
            temp.emplace_back(coin_types[i], i);
        }
        sort(temp.begin(), temp.end(), [](IntIx a, IntIx b) {
            return a.v < b.v;
        });
        vector<int> remap(coin_types.size());
        vector<int> coins(coin_types.size());
        for (int i = 0; i < coin_types.size(); i++) {
            remap[i] = temp[i].i;
            coins[i] = temp[i].v;
        }

        coins.erase(
                upper_bound(coins.begin(), coins.end(), value), coins.end());
        coins.push_back(value);
        const int n = coins.size();
        // c1 is representation of coins[i]-1, c2 representation of coins[i]. We
        // can add coins[i] to c1 variants.
        vector<int> c1(n, INF), c2(n, INF);
        vector<int> s1(n), s2(n);
        c1[0] = c2[0] = 0;
        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                auto d = div(coins[j] - coins[i], coins[i]);
                if (d.rem == 0) {
                    auto x = c1[i] + d.quot;
                    if (x <= c1[j]) {
                        c1[j] = x;
                        s1[j] = i;
                    }
                }
                if (d.rem == coins[i] - 1) {
                    auto x = c1[i] + d.quot + 1;
                    if (x <= c2[j]) {
                        c2[j] = x;
                        s2[j] = i;
                    }
                }
            }
        }
        vector<int> res(coin_types.size());
        auto k = s2[n - 1];
        res[remap[k]] = (value - coins[k] + 1) / coins[k];
        while (k != 0) {
            auto j = s1[k];
            res[remap[j]] = (coins[k] - coins[j]) / coins[j];
            k = j;
        }
        return res;
    }
};

int main(int argc, char** argv) {
    ios_base::sync_with_stdio(false);
    {
        auto sol = make_unique<ChangeOptimizer>();
        auto v = sol->fewestCoins({1, 10, 25}, 49);
        for (auto x : v) {
            cout << x << " ";
        }
        cout << endl;
    }
    {
        auto sol = make_unique<ChangeOptimizer>();
        auto v = sol->fewestCoins({1, 3, 6, 2}, 11);
        for (auto x : v) {
            cout << x << " ";
        }
        cout << endl;
    }
    return 0;
}
