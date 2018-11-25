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

using Point = pair<int, int>;

class CatchTheBeat {
public:
    int maxCatched(
            int n,
            int x0,
            int y0,
            int a,
            int b,
            int c,
            int d,
            int mod1,
            int mod2,
            int offset) {
        vector<int> x(n), y(n);
        x[0] = x0;
        for (int i = 1; i < n; i++)
            x[i] = (x[i - 1] * i64(a) + b) % mod1;
        for (int i = 0; i < n; i++)
            x[i] -= offset;
        y[0] = y0;
        for (int i = 1; i < n; i++)
            y[i] = (y[i - 1] * i64(c) + d) % mod2;
        vector<Point> p;
        // Turn -45 degree points (or 45 degree axis) to make sectors aligned
        // and do LIS.
        for (int i = 0; i < n; i++) {
            if (y[i] >= abs(x[i])) {
                p.emplace_back(y[i] + x[i], y[i] - x[i]);
            }
        }
        sort(p.begin(), p.end());
        vector<int> incr;
        int len = 0;
        for (int i = 0; i < p.size(); i++) {
            auto u = upper_bound(incr.begin(), incr.end(), p[i].second);
            incr.erase(u, incr.end()); // Same as resize.
            incr.push_back(p[i].second);
            if (incr.size() > len) len = incr.size();
        }
        return len;
    }
};

int main(int argc, char** argv) {
    ios_base::sync_with_stdio(false);
    {
        auto sol = make_unique<CatchTheBeat>();
        cout << sol->maxCatched(
                        10,
                        999999957,
                        79,
                        993948167,
                        24597383,
                        212151897,
                        999940854,
                        999999986,
                        999940855,
                        3404)
             << endl;
    }
    return 0;
}
