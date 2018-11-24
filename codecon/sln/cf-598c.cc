#include <algorithm>
#include <array>
#include <cmath>
#include <cstdio>
#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using pii = std::pair<int, int>;

const auto pi = acos(-1);

struct point {
    int x = 0;
    int y = 0;
    int q = 0;
    int i = 0;

    void set_q() {
        if (y == 0) {
            q = x >= 0 ? 0 : 4;
        } else if (x == 0) {
            q = y >= 0 ? 2 : 6;
        } else if (x > 0 && y > 0) {
            q = 1;
        } else if (x < 0 && y > 0) {
            q = 3;
        } else if (x < 0 && y < 0) {
            q = 5;
        } else if (x > 0 && y < 0) {
            q = 7;
        }
    }
};

namespace {
vector<point> pt;
}

i64 dp(int i, int j) {
    return i64(pt[i].x) * pt[j].x + i64(pt[i].y) * pt[j].y;
}

i64 cp(int i, int j) {
    return i64(pt[i].x) * pt[j].y - i64(pt[i].y) * pt[j].x;
}

bool delta_less(int i, int j) {
    int i1 = (i + 1) % pt.size();
    int j1 = (j + 1) % pt.size();
    auto dpi = dp(i, i1);
    auto cpi = abs(cp(i, i1));
    auto dpj = dp(j, j1);
    auto cpj = abs(cp(j, j1));
    if (dpi * dpj <= 0) {
        return dpi > dpj;
    }
    auto q = cpi * dpj - cpj * dpi;
    return dpi > 0 ? q < 0 : q < 0;
}

int main() {
    int n = 0;
    cin >> n;
    pt.resize(n);
    for (int i = 0; i < n; i++) {
        cin >> pt[i].x >> pt[i].y;
        pt[i].set_q();
        pt[i].i = i + 1;
    }
    sort(pt.begin(), pt.end(), [](auto a, auto b) {
        if (a.q != b.q) {
            return a.q < b.q;
        } else {
            // (ay / ax) ~ (by / bx), multiply by ax * bx > 0 (cos has same sign
            // in quarters).
            auto s = abs(a.y * b.x) - abs(b.y * a.x);
            if (a.q == 1 || a.q == 5) {
                return s < 0;
            } else {
                return s > 0;
            }
        }
    });
    // To compute angle use cross and dot prosuct:
    //      dp = |a| * |b| * cos phi
    //      cp = |a| * |b| * sin phi
    // (cp > 0 becuase we're interested in angles 0...pi). So tan phi = cp / dp.
    // If dp > 0 we have increasing function, dp < 0 - decreasing.
    int imin = 0;
    for (int i = 1; i < n; i++) {
        if (delta_less(i, imin)) {
            imin = i;
        }
    }
    cout << pt[imin].i << " " << pt[(imin + 1) % n].i << "\n";
    return 0;
}
