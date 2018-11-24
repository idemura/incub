#include <algorithm>
#include <cassert>
#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;

namespace {
using i64 = long long int;
using pii = pair<int, int>;

constexpr auto kMax = 0x7fffffff;

// char, length => cost
vector<int> cost_map[26];

void computeCosts(string const &s) {
    for (auto &v : cost_map) {
        v.resize(s.size() + 1, kMax);
        v[0] = 0;
    }
    for (int i = 0; i < s.size(); i++) {
        int counter[26] = {};
        for (int j = i; j < s.size(); j++) {
            auto chr = s[j] - 'a';
            counter[chr]++;
            auto l = j + 1 - i;
            cost_map[chr][l] = min(cost_map[chr][l], l - counter[chr]);
        }
        for (int chr = 0; chr < 26; chr++) {
            auto l = (int)s.size() - i;
            cost_map[chr][l] = min(cost_map[chr][l], l - counter[chr]);
        }
    }
    for (auto &v : cost_map) {
        for (int j = 1; j < v.size(); j++) {
            v[j] = min(v[j], v[j - 1] + 1);
        }
    }
}
} // namespace

int main() {
    int n, m;
    string s;
    cin >> n >> s >> m;
    assert(n == s.size());
    computeCosts(s);
    for (int i = 0; i < m; i++) {
        int k;
        char chr;
        cin >> k >> chr;
        auto const &vec = cost_map[chr - 'a'];
        auto pos = int(upper_bound(vec.begin(), vec.end(), k) - vec.begin());
        auto res = pos - 1 + k - vec[pos - 1];
        cout << min(res, n) << "\n";
    }
    return 0;
}
