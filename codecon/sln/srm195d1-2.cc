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
    C(const C &) = delete;                                                     \
    C &operator=(const C &) = delete;

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

struct StringIx {
    string w;
    int i;
    StringIx(string w, int i): w(move(w)), i(i) {}
    bool operator<(const StringIx &other) const {
        return w < other.w;
    }
};

int diff_index(const string &a, const string &b) {
    int i = 0;
    while (i < a.size() && a[i] == b[i])
        i++;
    return i;
}

class SimpleIO {
public:
    int worstCase(string pattern, int target_state) {
        const int n = pattern.size();
        string s = pattern + pattern;
        vector<StringIx> state_seq;
        for (int i = 0; i < n; i++) {
            state_seq.push_back(StringIx(s.substr(i + 1, n), i));
        }
        sort(state_seq.begin(), state_seq.end());
        int max_lex = 0;
        for (int i = 1; i < n; i++) {
            int k = diff_index(state_seq[i - 1].w, state_seq[i].w);
            if (k == n) return -1;
            int end_state = (state_seq[i].i + k + 1) % n;
            int len = k + 1 + (n + target_state - end_state) % n;
            if (len > max_lex) max_lex = len;
        }
        return max_lex;
    }
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    {
        auto sol = make_unique<SimpleIO>();
        cout << sol->worstCase("BNB", 0) << endl;
    }
    {
        auto sol = make_unique<SimpleIO>();
        cout << sol->worstCase("BNBNBNBN", 3) << endl;
    }
    {
        auto sol = make_unique<SimpleIO>();
        cout << sol->worstCase("BBNNBNBBBBNBBBBBB", 3) << endl;
    }
    return 0;
}
