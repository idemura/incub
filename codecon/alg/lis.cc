#include "base.h"

template <class T>
int lis(const vector<T> &v) {
    vector<T> incr;
    int len = 0;
    for (int i = 0; i < v.size(); i++) {
        auto u = upper_bound(incr.begin(), incr.end(), v[i]);
        incr.erase(u, incr.end()); // Same as resize.
        incr.push_back(v[i]);
        if (incr.size() > len) len = incr.size();
    }
    return len;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    vector<int> v{10, 30, 20, 15, 5, 8, 25, 25, 60, 40, 50};
    CHECK(lis(v) == 6);
    cout << "TESTS PASSED." << endl;
    return 0;
}
