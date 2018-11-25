#include <algorithm>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;

i64 mod(i64 n) {
    return n % (1'000'000'000 + 7);
}

i64 imin(i64 a, i64 b) {
    return std::min(a, b);
}

i64 imax(i64 a, i64 b) {
    return std::max(a, b);
}

// All x1, x2, y1, y2 are 0 based, right coordinates (x2 and y2) are exclusive.
// Quad of size qs at qx and qy. threshold (in) is the upper bound on number.
// Recursive quad tree walk.
i64 ranges_rect(
        int x1,
        int y1,
        int x2,
        int y2,
        int qx,
        int qy,
        i64 qs,
        i64 a,
        int threshold,
        int tabs) {
    if (x1 == x2 || y1 == y2) {
        return 0; // Empty
    }
    // if (x2 <= qx || x1 >= qx + qs || y2 <= qy || y1 >= qy + qs) {
    //     return 0;  // Outside
    // }
    i64 hs = qs / 2; // Half size
    i64 res = 0;
    // If one of dimension fully covered with quad, add a range.
    if (x2 - x1 == qs || y2 - y1 == qs) {
        // Numbers are [a + 1 .. a + hs], sum of them are sum of arithmetic
        // prograssion.
        if (a + 1 <= threshold) {
            i64 count = imin(threshold - a, qs);
            i64 ap_sum = mod(((a + 1) + (a + count)) * count / 2);
            // min(...) row/col [a + 1 .. a + count] replicated.
            res = ap_sum * imin(x2 - x1, y2 - y1);
        }
    } else {
        res += ranges_rect(
                imin(x1, qx + hs),
                imin(y1, qy + hs),
                imin(x2, qx + hs),
                imin(y2, qy + hs),
                qx,
                qy,
                hs,
                a,
                threshold,
                tabs + 1);
        res += ranges_rect(
                imax(x1, qx + hs),
                imin(y1, qy + hs),
                imax(x2, qx + hs),
                imin(y2, qy + hs),
                qx + hs,
                qy,
                hs,
                a + hs,
                threshold,
                tabs + 1);
        res += ranges_rect(
                imin(x1, qx + hs),
                imax(y1, qy + hs),
                imin(x2, qx + hs),
                imax(y2, qy + hs),
                qx,
                qy + hs,
                hs,
                a + hs,
                threshold,
                tabs + 1);
        res += ranges_rect(
                imax(x1, qx + hs),
                imax(y1, qy + hs),
                imax(x2, qx + hs),
                imax(y2, qy + hs),
                qx + hs,
                qy + hs,
                hs,
                a,
                threshold,
                tabs + 1);
    }
    return mod(res);
}

int main() {
    int n;
    cin >> n;
    for (int i = 0; i < n; i++) {
        int x1, y1, x2, y2, k;
        cin >> x1 >> y1 >> x2 >> y2 >> k;
        cout << ranges_rect(
                        x1 - 1,
                        y1 - 1,
                        x2,
                        y2,
                        0,
                        0,
                        i64(1) << 31,
                        0, // a
                        k, // threshold
                        0)
             << endl;
    }
    return 0;
}
