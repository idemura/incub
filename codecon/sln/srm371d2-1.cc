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

using namespace std;

typedef long long int lli;

class CondorcetVoting {
public:
    CondorcetVoting() {}

    int winner(const vector<string> &votes) {
        const int n = votes.size();
        const int m = votes[0].size();
        // Check every candidate to be winner.
        for (int i = 0; i < m; i++) {
            int n_pref = 0;
            for (int j = 0; j < m; j++) {
                if (i == j) continue;
                int x = 0, y = 0;
                for (int k = 0; k < n; k++) {
                    x += votes[k][i] < votes[k][j];
                    y += votes[k][i] > votes[k][j];
                }
                n_pref += x > y;
            }
            if (n_pref == m - 1) {
                return i;
            }
        }
        return -1;
    }
};

int main(int argc, char **argv) {
    vector<string> v;
    v.push_back("abcd");
    v.push_back("abcd");
    v.push_back("abcd");
    v.push_back("abcd");
    v.push_back("abcd");
    v.push_back("abcd");
    v.push_back("cbad");
    v.push_back("cbad");
    v.push_back("cbad");
    v.push_back("cbad");
    v.push_back("cbad");
    v.push_back("dbca");
    v.push_back("cbda");
    v.push_back("cbda");
    // v.push_back("abc");
    // v.push_back("bca");
    // v.push_back("cab");

    printf("%d\n", CondorcetVoting().winner(v));
    return 0;
}
