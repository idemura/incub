#include <cassert>
#include <iostream>
#include <limits>
#include <vector>

using namespace std;

double median(vector<int> const &a) {
    if (a.size() % 2 == 0) {
        return (a[a.size() / 2] + a[a.size() / 2 - 1]) * 0.5;
    }
    return a[a.size() / 2];
}

double
median_sorted_arrays(vector<int> const &nums1, vector<int> const &nums2) {
    if (nums1.size() == 0) {
        return median(nums2);
    }
    if (nums2.size() == 0) {
        return median(nums1);
    }
    int ts = nums1.size() + nums2.size();
    int i1 = 0;
    int i2 = ts / 2;
    if (i2 > nums1.size()) {
        i2 = nums1.size();
    }
    while (i1 < i2) {
        int j1 = (i1 + i2) / 2;
        int j2 = ts / 2 - j1;
        if (j2 > nums2.size()) {
            i1 = j1 + 1;
            continue;
        }
        if (j1 < nums1.size() && j2 > 0 && nums1[j1] <= nums2[j2 - 1]) {
            i1 = j1 + 1;
            continue;
        }
        if (j1 > 0 && j2 < nums2.size() && nums2[j2] <= nums1[j1 - 1]) {
            i2 = j1;
            continue;
        }
        i1 = j1;
        break;
    }
    int m1 = i1;
    int m2 = ts / 2 - m1;
    int prev = numeric_limits<int>::min();
    if (m1 > 0) {
        prev = max(nums1[m1 - 1], prev);
    }
    if (m2 > 0) {
        prev = max(nums2[m2 - 1], prev);
    }
    int next = numeric_limits<int>::max();
    if (m1 < nums1.size()) {
        next = min(nums1[m1], next);
    }
    if (m2 < nums2.size()) {
        next = min(nums2[m2], next);
    }
    if (ts % 2 == 0) {
        return (prev + next) * 0.5;
    }
    return next;
}

int main() {
    assert(median_sorted_arrays(vector<int>{1, 3}, vector<int>{2}) == 2);
    assert(median_sorted_arrays(vector<int>{2}, vector<int>{1, 3}) == 2);
    assert(median_sorted_arrays(vector<int>{1, 3}, vector<int>{2, 4}) == 2.5);
    assert(median_sorted_arrays(vector<int>{2, 4}, vector<int>{1, 3}) == 2.5);
    assert(median_sorted_arrays(vector<int>{1}, vector<int>{1}) == 1);
    assert(median_sorted_arrays(vector<int>{1, 2}, vector<int>{}) == 1.5);
    assert(median_sorted_arrays(vector<int>{}, vector<int>{1, 2}) == 1.5);
    assert(median_sorted_arrays(vector<int>{1}, vector<int>{2, 3, 4, 5}) == 3);
    assert(median_sorted_arrays(vector<int>{2}, vector<int>{1, 3, 4, 5}) == 3);
    assert(median_sorted_arrays(vector<int>{3}, vector<int>{1, 2, 4, 5}) == 3);
    assert(median_sorted_arrays(vector<int>{4}, vector<int>{1, 2, 3, 5}) == 3);
    assert(median_sorted_arrays(vector<int>{5}, vector<int>{1, 2, 3, 4}) == 3);
    cout << "TESTS PASSED." << endl;
    return 0;
}
