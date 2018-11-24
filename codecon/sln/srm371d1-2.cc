#include <algorithm>
#include <assert.h>
#include <map>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

typedef long long int lli;

class ChessMatchup {
public:
    static const int kDim = 52;
    int m[kDim][kDim];
    std::vector<int> t1, t2;

    ChessMatchup(): m() {}

    // Sort by skill both teams. Consider top players from two teams. Can prove
    // that following is true:
    //  1. If our player skill is higher, play and win, +2.
    //  2. If our player skill is lower, play our lowest skill player with the
    //     opponent.
    //  3. If skill are equal, try both cases (example: 7, 3 and 7, 2: better to
    //     play and draw, while 7, 2 and 7, 3: better to choose worst player).
    // I did top-bottom DP and than I realized only indices top player matter.
    // And I made it bottom-up: consider i(us) and j(them) indices, i >= j. We
    // depend only on i-1,j i-1,j-1 i,j-1. Note we can sort decreasing order for
    // bottom-up DP.
    int maximumScore(const std::vector<int> &us, const std::vector<int> &them) {
        t1 = us;
        t2 = them;
        std::sort(t1.begin(), t1.end());
        std::sort(t2.begin(), t2.end());
        return dp(t1.size());
    }

    int dp(int n) {
        // `i` is index in `t1`. So for indices we have `i <= j`.
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= i; j++) {
                if (t1[i] == t2[j]) {
                    m[i][j] = std::max(m[i - 1][j - 1] + 1, m[i][j - 1]);
                } else if (t1[i] > t2[j]) {
                    m[i][j] = m[i - 1][j - 1] + 2;
                } else {
                    m[i][j] = m[i][j - 1];
                }
            }
        }
        return m[n][n];
    }
};

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    std::vector<int> us, them;
    us.push_back(1);
    us.push_back(10);
    us.push_back(7);
    us.push_back(4);
    them.push_back(15);
    them.push_back(3);
    them.push_back(8);
    them.push_back(7);
    printf("%d\n", ChessMatchup().maximumScore(us, them));
    return 0;
}
