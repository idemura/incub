#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

int main(int argc, char **argv) {
    int n;
    scanf("%d", &n);
    vector<int> a(n);
    i64 sum = 0;
    unordered_map<i64, vector<int>> counter;
    for (int i = 0; i < n; i++) {
        scanf("%d", &a[i]);
        sum += a[i];
        counter[a[i]].push_back(i);
    }
    unordered_set<int> ans;
    for (int i = 0; i < n; i++) {
        // sum - a[i] - sum of the rest and it should be equal to a[i].
        // delta is who we need to remove.
        i64 delta = sum - 2 * a[i];
        auto itr = counter.find(delta);
        if (itr == counter.end()) {
            continue;
        }
        for (auto j : itr->second) {
            if (j != i) {
                ans.insert(j);
            }
        }
    }
    printf("%d\n", (int)ans.size());
    for (auto k : ans) {
        printf("%d ", k + 1);
    }
    printf("\n");
    return 0;
}
