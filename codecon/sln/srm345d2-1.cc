#include <algorithm>
#include <assert.h>
#include <ctype.h>
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

class Trekking {
public:
    Trekking() {}

    int
    findCamps(const std::string &trail, const std::vector<std::string> &plans) {
        int min_nights = INF;
        for (int i = 0; i < plans.size(); i++) {
            int n = 0;
            if (getCampStay(trail, plans[i], &n)) {
                min_nights = std::min(n, min_nights);
            }
        }
        return min_nights == INF ? -1 : min_nights;
    }

    bool
    getCampStay(const std::string &trail, const std::string &plan, int *len) {
        *len = 0;
        for (int i = 0; i < plan.size(); i++) {
            if (plan[i] == 'C') {
                if (trail[i] == '^') {
                    return false;
                } else {
                    (*len)++;
                }
            }
        }
        return true;
    }
};

int main(int argc, char **argv) {
    // #ifndef ONLINE_JUDGE
    //   freopen("in", "r", stdin);
    // #endif
    std::vector<std::string> plans;
    plans.push_back("CwwCwwCwwCww");
    plans.push_back("wwwCwCwwwCww");
    plans.push_back("wwwwCwwwwCww");
    int r = Trekking().findCamps("^^....^^^...", plans);
    printf("%d\n", r);
    return 0;
}
