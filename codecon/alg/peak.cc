#include "base.h"

// Search in [1,-2], first and last are assumed -1, others >= 0.
// Peak is a[i] > a[i-1] and a[i+1].
// Important: a[i] != a[i+1]. Find one(arbitrary) peak.
int find_peak(const vector<int> &a) {
    assert(a.size() >= 3);
    assert(a[0] == -1 && a[a.size() - 1] == -1);

    int i = 1, j = a.size() - 2;
    while (i != j) {
        int m = (i + j) / 2;
        if (a[m - 1] > a[m]) {
            j = m - 1;
        } else {
            if (a[m + 1] > a[m]) {
                i = m + 1;
            } else {
                return m;
            }
        }
    }
    return i;
}

void test_peak(vector<int> a) {
    int i = find_peak(a);
    if (i <= 0 || i >= a.size() - 1) {
        cout << "Invalid index " << i << endl;
        return;
    }
    auto peak = a[i] > a[i - 1] && a[i] > a[i + 1];
    if (!peak) {
        cout << "Not a peak at " << i << ": " << a[i - 1] << " " << a[i] << " "
             << a[i + 1] << endl;
        exit(EXIT_FAILURE);
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test_peak({-1, 1, -1});
    test_peak({-1, 1, 2, -1});
    test_peak({-1, 2, 1, -1});
    test_peak({-1, 1, 2, 3, 1, 2, -1});
    test_peak({-1, 1, 2, 3, 4, 5, -1});
    test_peak({-1, 1, 3, 1, 2, 4, 6, -1});
    test_peak({-1, 1, 3, 1, 2, 4, 3, -1});
    test_peak({-1, 1, 3, 4, 2, 4, 3, -1});
    cout << "TESTS PASSED." << endl;
    return 0;
}
