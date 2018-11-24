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

class FanFailure {
public:
    using VectorInt = vector<int>;

    vector<int> getRange(const vector<int> &cap, int min_cooling) {
        // cooling -> last value in the array of indices producing this cooling.
        unordered_map<int, int> c, next;
        c[0] = -1;
        int mfs = -1, mfc = -1;
        int set_size = 1;
        do {
            next.clear();
            int g = 0;
            for (auto kv : c) {
                for (int i = kv.second + 1; i < cap.size(); i++) {
                    auto new_cooling = kv.first + cap[i];
                    if (new_cooling >= min_cooling) {
                        g++;
                        if (mfs < 0) {
                            mfs = cap.size() - set_size;
                        }
                    }
                    auto it = next.find(new_cooling);
                    if (it == next.end()) {
                        next[new_cooling] = i;
                    } else {
                        if (i < it->second) it->second = i;
                    }
                }
            }
            if (g == next.size() && mfc < 0) {
                mfc = cap.size() - set_size;
            }
            next.swap(c);
            set_size++;
        } while (!c.empty());
        return {mfs, mfc};
    }
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    {
        auto sol = make_unique<FanFailure>();
        auto v = sol->getRange({1, 2, 3}, 2);
        cout << v[0] << " " << v[1] << endl;
    }
    {
        auto sol = make_unique<FanFailure>();
        auto v = sol->getRange({8, 5, 6, 7}, 22);
        cout << v[0] << " " << v[1] << endl;
    }
    {
        auto sol = make_unique<FanFailure>();
        auto v = sol->getRange(
                {676, 11, 223, 413, 823, 122, 547, 187, 28}, 1000);
        cout << v[0] << " " << v[1] << endl;
    }
    return 0;
}
