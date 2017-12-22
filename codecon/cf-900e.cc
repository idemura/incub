#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <algorithm>
#include <map>
#include <vector>
#include <string>
#include <unordered_map>
#include <utility>

#include "log.hpp"

using namespace std;

using i32 = int32_t;
using i64 = int64_t;
using pii = std::pair<int, int>;

constexpr uint32_t kSize{100008};
char str[kSize];
int dp_score[kSize];
int dp_subst[kSize];

int main() {
    int n = 0, m = 0;
    scanf("%d%s%d", &n, str, &m);
    int count_a[2]{};
    int count_b[2]{};
    int count_q = 0;
    dlog<<str;
    for (int i = 0; i < n;) {
        if (i >= m) {
            dlog<<"kick out "<<(i - m);
            switch (str[i - m]) {
            case 'a':
                count_a[(i - m) % 2]--;
                break;
            case 'b':
                count_b[(i - m) % 2]--;
                break;
            case '?':
                count_q--;
                break;
            }
        }
        switch (str[i]) {
        case 'a':
            count_a[i % 2]++;
            break;
        case 'b':
            count_b[i % 2]++;
            break;
        case '?':
            count_q++;
            break;
        }
        i++;
        // dp_* are 1-based, or length
        if (i >= m) {
            auto start = i - m;
            dlog<<"check start "<<start<<" i "<<i;
            dlog<<"count a: "<<count_a[0]<<" "<<count_a[1];
            dlog<<"count b: "<<count_b[0]<<" "<<count_b[1];
            dp_score[i] = dp_score[i - 1];
            dp_subst[i] = dp_subst[i - 1];
            dlog<<"prev score "<<dp_score[i]<<" subst "<<dp_subst[i];
            dlog<<"start score "<<dp_score[start]<<" subst "<<dp_subst[start];
            if (count_a[1 - start % 2] == 0 &&
                    count_b[start % 2] == 0 &&
                    dp_score[start] + 1 >= dp_score[i]) {
                dlog<<"consider to bump";
                if (dp_score[start] + 1 > dp_score[i] ||
                        dp_subst[start] + count_q < dp_subst[i]) {
                    dp_score[i] = dp_score[start] + 1;
                    dp_subst[i] = dp_subst[start] + count_q;
                    dlog<<"bumping to score "<<dp_score[i]<<" subst "<<dp_subst[i];
                }
            }
        }
    }
    dlog<<"final score "<<dp_score[n];
    dlog<<"final subst "<<dp_subst[n];
    printf("%d\n", dp_subst[n]);
    return 0;
}
