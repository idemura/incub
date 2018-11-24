#include <algorithm>
#include <assert.h>
#include <ctype.h>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C&);                                                               \
    C& operator=(const C&);

using namespace std;

typedef long long int i64;

constexpr int INF = 0x7fffffff;
constexpr int DIM = 200000;

vector<int> prefix_fn(const string& s) {
    vector<int> pf(s.size());
    // j is length of matching prefix up to (not including) char i.
    int i = 1, j = 0;
    while (i < s.size()) {
        if (s[i] == s[j]) {
            j++;
            i++;
            pf[i] = j;
        } else {
            if (!pf[i]) pf[i] = j; // Set one(first) time.
            if (j == 0) {
                i++;
            } else {
                j = pf[j];
            }
        }
    }
    pf.push_back(j);
    return pf;
}

bool check(int n, int k) {
    auto q = n / k;
    return q >= k - 1 || n % k <= q;
}

int main(int argc, char** argv) {
    ios_base::sync_with_stdio(false);
    int n, k;
    string s;
    cin >> n >> k >> s;
    if (k == 1) {
        cout << string(s.size(), '1') << endl;
        return 0;
    }
    const auto& pf = prefix_fn(s);
    string res(s.size(), 0);
    for (int i = 1; i < pf.size(); i++) {
        if (pf[i] == 0) {
            res[i - 1] = '0';
            continue;
        }
        int repeated_len = i - pf[i];
        int parts = i / repeated_len;
        if (i % repeated_len == 0) {
            res[i - 1] = check(parts, k) ? '1' : '0';
        } else {
            if (parts < k) {
                res[i - 1] = '0';
            } else {
                res[i - 1] = check(parts - k, k) ? '1' : '0';
            }
        }
    }
    cout << res << endl;
    return 0;
}
