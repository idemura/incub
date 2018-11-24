#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <math.h>
#include <memory>
#include <queue>
#include <sstream>
#include <stdlib.h>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C)                                                        \
    C(const C&) = delete;                                                      \
    C& operator=(const C&) = delete;

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

class SumOfSelectedCells {
public:
    string hypothesis(const vector<string>& table) {
        const int n1 = table.size();
        vector<vector<int>> m(n1);
        for (int i = 0; i < n1; i++) {
            for (int j = 0; j != string::npos;) {
                auto k = table[i].find(' ', j);
                if (k == string::npos) {
                    m[i].push_back(stoi(table[i].substr(j)));
                    break;
                } else {
                    m[i].push_back(stoi(table[i].substr(j, k)));
                    j = k + 1;
                }
            }
        }
        const int n2 = m[0].size();
        for (int i = 0; i < n1; i++) {
            for (int j = i + 1; j < n1; j++) {
                int d = m[i][0] - m[j][0];
                for (int k = 1; k < n2; k++) {
                    if (m[i][k] - m[j][k] != d) {
                        return "INCORRECT";
                    }
                }
            }
        }
        for (int i = 0; i < n2; i++) {
            for (int j = i + 1; j < n2; j++) {
                int d = m[0][i] - m[0][j];
                for (int k = 1; k < n1; k++) {
                    if (m[k][i] - m[k][j] != d) {
                        return "INCORRECT";
                    }
                }
            }
        }
        return "CORRECT";
    }
};

int main() {
    cout << make_unique<SumOfSelectedCells>()->hypothesis({"11 12 13 14",
                                                           "21 22 23 24",
                                                           "31 32 33 34",
                                                           "41 42 43 44"})
         << endl;
    cout << make_unique<SumOfSelectedCells>()->hypothesis({"3 7", "3 7", "3 7"})
         << endl;
    return 0;
}
