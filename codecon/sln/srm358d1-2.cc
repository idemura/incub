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

int gcd(int a, int b) {
    while (b != 0) {
        int t = a % b;
        a = b;
        b = t;
    }
    return a;
}

class BalanceScale {
public:
    int takeWeights(vector<int> weight) {
        auto g = weight[0];
        for (int i = 1; i < weight.size(); i++) {
            if (g == 1) break;
            g = gcd(weight[i], g);
        }
        if (g > 1) {
            for (auto& x : weight) {
                x /= g;
            }
        }
        // `subset_gcds` is a set of gcd of subsets of size 1,2,...n elements of
        // the array `weight`.
        unordered_map<int, bool> subset_gcds;
        for (auto x : weight)
            subset_gcds[x] = true;
        // k is subset size.
        for (int k = 1; true; k++) {
            unordered_map<int, bool> t;
            for (auto g : subset_gcds) {
                if (g.first == 1) return k;
                for (auto m : weight) {
                    t[gcd(m, g.first)] = true;
                }
            }
            subset_gcds = move(t);
        }
        return 0; // Never here.
    }
};

int main() {
    cout << make_unique<BalanceScale>()->takeWeights({5, 4, 1, 8}) << endl;
    cout << make_unique<BalanceScale>()->takeWeights({2, 3, 8, 9}) << endl;
    return 0;
}
