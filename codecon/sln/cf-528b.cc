#include <algorithm>
#include <assert.h>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <set>
#include <sstream>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &) = delete;                                                     \
    C &operator=(const C &) = delete;

using namespace std;

using i64 = long long int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int DIM = 0;

struct RightEnd {
    int l = 0, r = 0;
    bool operator<(const RightEnd &other) const {
        return r < other.r;
    }
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int n;
    cin >> n;
    vector<RightEnd> right(n);
    for (int i = 0; i < n; i++) {
        int x, w;
        cin >> x >> w;
        RightEnd re;
        re.l = x - w;
        re.r = x + w;
        right[i] = re;
    }
    sort(right.begin(), right.end());
    int c = 0, x = -INF;
    for (auto &re : right) {
        if (re.l >= x) {
            c++;
            x = re.r;
        }
    }
    cout << c << endl;
    return 0;
}
