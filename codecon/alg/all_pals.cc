#include <algorithm>
#include <assert.h>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <sstream>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C &) = delete;                                                     \
    C &operator=(const C &) = delete;

#define CHECK(E)                                                               \
    do {                                                                       \
        if (!(E)) {                                                            \
            cout << "CHECK failed at " << __FILE__ << "@" << __LINE__ << endl; \
            exit(EXIT_FAILURE);                                                \
        }                                                                      \
    } while (false)

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

using Idx2i = pair<int, int>;

void insert_pal(unordered_set<string> &all_p, const string &s, int i0, int i1) {
    auto p = s.substr(i0, i1 - i0 + 1);
    all_p.insert(p);
}

// Range with both ends included.
unordered_set<string> all_palindromes(const string &s) {
    unordered_set<string> all_p;
    unordered_set<int> b1, b2; // Set of start indices.
    b1.insert(1);
    insert_pal(all_p, s, 0, 0);
    for (int i = 1; i < s.size(); i++) {
        // Second is the empty substring on the next iteration.
        b2.insert({i, i + 1});
        insert_pal(all_p, s, i, i);
        for (int j = 1; j <= i; j++) {
            if (b1.find(j) != b1.end() && s[i] == s[j - 1]) {
                b2.insert(j - 1);
                insert_pal(all_p, s, j - 1, i);
            }
        }
        b1 = move(b2);
        b2 = unordered_set<int>();
    }
    return move(all_p);
}

void test(const string &s) {
    cout << "Palindromes of " << s << endl;
    for (auto &s : all_palindromes(s)) {
        cout << s << endl;
    }
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    test("abcd");
    test("abaaa");
    test("babcacac");
    cout << "TESTS PASSED." << endl;
    return 0;
}
