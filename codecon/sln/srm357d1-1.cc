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

class Hotel {
public:
    int marketCost(
            int min_customers,
            const vector<int> &customers,
            const vector<int> &cost) {
        constexpr int DIM = 1001;
        const int n = cost.size();
        vector<int> total_customers(DIM, -1);
        total_customers[0] = 0;
        int max_bought = -1;
        for (int i = 0; i < DIM; i++) {
            if (total_customers[i] < 0 || total_customers[i] <= max_bought)
                continue;
            if (total_customers[i] >= min_customers) {
                return i;
            }
            max_bought = total_customers[i];
            for (int j = 0; j < n; j++) {
                auto k = i + cost[j];
                if (k >= DIM) continue;
                if (total_customers[i] + customers[j] > total_customers[k]) {
                    total_customers[k] = total_customers[i] + customers[j];
                }
            }
        }
        return -1;
    }
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    {
        Hotel sol;
        cout << sol.marketCost(10, {1, 2, 3}, {3, 2, 1}) << endl;
    }
    {
        Hotel sol;
        cout << sol.marketCost(
                        10,
                        {1, 2, 3, 4, 5, 6, 7, 8, 9, 10},
                        {1, 2, 3, 4, 5, 6, 7, 8, 9, 10})
             << endl;
    }
    {
        Hotel sol;
        cout << sol.marketCost(12, {5, 1}, {3, 1}) << endl;
    }
    {
        Hotel sol;
        cout << sol.marketCost(100, {9, 11, 4, 7, 2, 8}, {4, 9, 3, 8, 1, 9})
             << endl;
    }
    return 0;
}
