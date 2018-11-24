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
#include <unordered_set>
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
    int n = 0;
    cin >> n;
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        cin >> a[i];
    }
    sort(a.begin(), a.end());
    unordered_set<int> sums;
    for (int i = 0; i < n; i++) {
        for (int j = i + 1; j < n; j++) {
            sums.insert(a[i] + a[j]);
        }
    }
    int max_pair_n = 0;
    for (auto s : sums) {
        int pair_n = 0;
        for (int i = 0, j = n - 1; i < j;) {
            if (a[i] + a[j] < s)
                i++;
            else if (a[i] + a[j] > s)
                j--;
            else {
                i++;
                j--;
                pair_n++;
            }
        }
        max_pair_n = max(max_pair_n, pair_n);
    }
    cout << 2 * max_pair_n << endl;
    return 0;
}
