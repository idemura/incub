#include "base.h"

// Given an array of integers. We know that one integer occurs more than n/2
// times. Find this major element.
// Voting algorithm.
int find_dominant(const vector<int> &a) {
    int x = 0;
    int l = 0;
    for (auto e : a) {
        if (l == 0) {
            x = e;
            l = 1;
        } else if (x == e) {
            l++;
        } else {
            l--;
        }
    }
    // Assume element is present.
    if (false) {
        int c = 0;
        for (auto e : a) {
            if (e == x) c++;
        }
        if (c < (a.size() + 1) / 2) {
            x = 0;
        }
    }
    return x;
}

int main(int argc, char **argv) {
    // ios_base::sync_with_stdio(false);
    CHECK(1 == find_dominant({1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 1, 1}));
    CHECK(1 == find_dominant({1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1}));
    CHECK(1 == find_dominant({1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1}));
    CHECK(1 == find_dominant({1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 1, 3}));
    CHECK(1 == find_dominant({2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1}));
    CHECK(1 == find_dominant({1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2}));
    cout << "TESTS PASSED." << endl;
    return 0;
}
