#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <random>
#include <sstream>
#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000009;

using ColorPos = pair<char, int>;

int solve(vector<ColorPos> a) {
    // if (a.empty()) return 0;
    sort(a.begin(), a.end(), [](ColorPos a, ColorPos b) {
        return a.second < b.second;
    });
    i64 r = 1;
    for (int i = 1; i < a.size(); i++) {
        if (a[i - 1].first == a[i].first) {
        } else {
            r = (r * (a[i].second - a[i - 1].second)) % MOD;
        }
    }
    return r;
}

void print(const vector<char> &a) {
    for (auto x : a) {
        printf("%c", x == 0 ? '_' : x);
    }
    printf("\n");
}

int main(int argc, char **argv) {
    int t = 0;
    scanf("%d", &t);
    while (t--) {
        int n, m;
        scanf("%d%d", &n, &m);
        vector<ColorPos> a(m);
        for (int i = 0; i < m; i++) {
            scanf(" %c%d", &a[i].first, &a[i].second);
        }
        printf("%d\n", solve(move(a)));
    }
    return 0;
}
