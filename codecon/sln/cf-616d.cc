#include <algorithm>
#include <cmath> // Overloads for abs.
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <random>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

int main(int argc, char **argv) {
    // ios_base::sync_with_stdio(false);
    int n = 0, k = 0;
    scanf("%d%d", &n, &k);
    vector<int> a(n);
    for (int i = 0; i < n; i++) {
        scanf("%d", &a[i]);
    }
    vector<int> right_pos(1000005, -1);
    int right_pos_size = 0;
    int longest_l = 0, longest_r = 0;
    int l = 0;
    for (int i = 0; i < n; i++) {
        auto rp = right_pos[a[i]];
        if (rp < 0) {
            // See a[i] first time (since @l). Increase cardinality.
            if (right_pos_size == k) {
                // Pull left end to right maintaining longest possible until
                // cardinality drops by one.
                for (; l < i; l++) {
                    if (right_pos[a[l]] == l) {
                        right_pos[a[l++]] = -1;
                        right_pos_size--;
                        break;
                    }
                }
            }
            right_pos_size++;
        }
        right_pos[a[i]] = i;
        if (i - l > longest_r - longest_l) {
            longest_l = l;
            longest_r = i;
        }
    }
    printf("%d %d\n", longest_l + 1, longest_r + 1);
    return 0;
}
