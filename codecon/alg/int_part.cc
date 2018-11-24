#include "base.h"

// How many ways I can represent n as sum? Say, 4=1+1+2 (same as 2+1+1!) and
// so on.
// `mem` should be a table, of course.
// http://en.wikipedia.org/wiki/Partition_%28number_theory%29
i64 count_groups_mem(map<pair<int, int>, i64> &mem, int n, int c) {
    if (c > n) return 0;
    auto it = mem.find(make_pair(n, c));
    if (it != mem.end()) {
        return it->second;
    }
    // Either take c or move forward.
    auto &s = mem[make_pair(n, c)];
    if (c == n)
        s = 1;
    else
        s = count_groups_mem(mem, n - c, c) + count_groups_mem(mem, n, c + 1);
    return s;
}

i64 count_groups(int n) {
    map<pair<int, int>, i64> mem;
    return count_groups_mem(mem, n, 1);
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    CHECK(count_groups(4) == 5);
    CHECK(count_groups(5) == 7);
    CHECK(count_groups(6) == 11);
    CHECK(count_groups(7) == 15);
    CHECK(count_groups(8) == 22);
    CHECK(count_groups(9) == 30);
    CHECK(count_groups(10) == 42);
    CHECK(count_groups(11) == 56);
    cout << "TESTS PASSED." << endl;
    return 0;
}
