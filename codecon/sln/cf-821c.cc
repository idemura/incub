#include <algorithm>
#include <cstdio>
#include <unordered_map>
#include <vector>

using namespace std;

using i64 = long long int;

int main() {
    char buf[80] = {};
    int n = 0;
    scanf("%d", &n);
    vector<int> stack, seq_n(3 * 100'000 + 10);
    auto insert = 0;
    auto r = 0;
    auto reorder = 0;
    auto reorderCount = 0;
    for (int i = 0; i < 2 * n; i++) {
        scanf("%s", buf);
        if (buf[0] == 'a') {
            int k = 0;
            scanf("%d", &k);
            insert++;
            stack.push_back(k);
            seq_n[k] = insert;
        } else {
            r++;
            auto inc_reorder = false;
            if (stack.empty()) {
                inc_reorder = seq_n[r] > reorder;
            } else {
                if (stack.back() == r) {
                    stack.pop_back();
                } else {
                    inc_reorder = true;
                }
            }
            if (inc_reorder) {
                stack.clear();
                reorderCount++;
                reorder = insert;
            }
        }
    }
    printf("%d\n", reorderCount);
    return 0;
}
