#include <algorithm>
#include <array>
#include <cmath>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using pii = std::pair<int, int>;

struct IntMap {
    void set(int i, char v) {
        if (i >= 0) {
            if (p.size() <= i) {
                p.resize(i + 1);
            }
            p[i] = v;
        } else {
            i *= -1;
            if (n.size() <= i) {
                n.resize(i + 1);
            }
            n[i] = v;
        }
    }

    template <class F>
    void for_each(F f) const {
        for (size_t i = 1; i < n.size(); i++) {
            auto j = n.size() - i;
            if (n[j]) f(-(int)j, n[j]);
        }
        for (size_t i = 0; i < p.size(); i++) {
            if (p[i]) f((int)i, p[i]);
        }
    }

    bool contains(int k) const {
        if (k >= 0)
            return k < p.size();
        else
            return -k < n.size();
    }

    char get(int k) const {
        if (k >= 0) {
            return p[k];
        } else {
            k *= -1;
            return n[k];
        }
    }

    vector<char> n, p;
};

namespace {
IntMap im[1002];
}

int main() {
    int n = 0, k = 0;
    string s;
    cin >> n >> k >> s;
    im[0].set(0, 'D');
    for (int i = 0; i < s.size(); i++) {
        auto bound = (i + 1 == s.size() ? k : k - 1);
        if (s[i] == 'D') {
            im[i].for_each(
                    [& m = im[i + 1], bound](int j, int v) { m.set(j, 'D'); });
        } else if (s[i] == 'L') {
            im[i].for_each([& m = im[i + 1], bound](int j, int v) {
                if (abs(j - 1) <= bound) {
                    m.set(j - 1, 'L');
                }
            });
        } else if (s[i] == 'W') {
            im[i].for_each([& m = im[i + 1], bound](int j, int v) {
                if (abs(j + 1) <= bound) {
                    m.set(j + 1, 'W');
                }
            });
        } else {
            im[i].for_each([& m = im[i + 1], bound](int j, int v) {
                m.set(j, 'D');
                if (abs(j - 1) <= bound) {
                    m.set(j - 1, 'L');
                }
                if (abs(j + 1) <= bound) {
                    m.set(j + 1, 'W');
                }
            });
        }
    }
    if (im[n].contains(k)) {
        // Pass
    } else if (im[n].contains(-k)) {
        k *= -1;
    } else {
        cout << "NO\n";
        return 0;
    }
    char t[1002] = {};
    while (n > 0) {
        auto c = t[n - 1] = im[n].get(k);
        switch (c) {
        case 'W':
            k--;
            break;
        case 'L':
            k++;
            break;
        }
        n--;
    }
    cout << t << "\n";
    return 0;
}
