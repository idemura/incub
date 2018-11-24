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

int main() {
    ios_base::sync_with_stdio(false);
    int t;
    cin >> t;
    while (t-- > 0) {
        int n, a;
        cin >> n;
        vector<int> h(100002);
        int max_equal = 0;
        for (int i = 0; i < n; i++) {
            cin >> a;
            max_equal = max(max_equal, ++h[a]);
        }
        cout << (n - max_equal) << endl;
    }
    return 0;
}
