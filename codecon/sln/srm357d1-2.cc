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

class WebsiteRank {
public:
    i64 countVotes(const vector<string> &votes, string website) {
        vector<vector<int>> vi;
        for (auto &s : votes) {
            vi.emplace_back(parse(s));
        }
        const int n = ix_map_.size();
        create_m(n);

        for (auto &a : vi) {
            for (int i = 1; i < a.size(); i++) {
                m_[a[0]][a[i]] = 1;
            }
        }
        // Do Floyd-Warshall to decide about cycles (connection, not path
        // length).
        m_closure_ = m_;
        for (int k = 0; k < n; k++) {
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < n; j++) {
                    m_closure_[i][j] = m_closure_[i][j] ||
                            (m_closure_[i][k] && m_closure_[k][j]);
                }
            }
        }
        return dfs(ix_map_[website]);
    }

    i64 dfs(int v) {
        int s = 1;
        for (int i = 0; i < m_.size(); i++) {
            // If we not connected, or have a loop back, skip it.
            if (m_[v][i] == 0 || m_closure_[i][v]) continue;
            s += dfs(i);
        }
        return s;
    }

    vector<int> parse(const string &s) {
        vector<int> r;
        for (int i = 0, j = 0; j != string::npos; i = j + 1) {
            j = s.find(' ', i);
            int ix = 0;
            auto ss = s.substr(i, j == string::npos ? string::npos : j - i);
            auto it = ix_map_.find(ss);
            if (it == ix_map_.end()) {
                // If I do on the next like, TopCoder assigns 1 to the first
                // string.
                auto ss_ix = ix_map_.size();
                ix = ix_map_[ss] = ss_ix;
            } else {
                ix = it->second;
            }
            r.push_back(ix);
        }
        return r;
    }

    void create_m(int n) {
        m_.resize(n);
        for (auto &a : m_)
            a.resize(n);
    }

    unordered_map<string, int> ix_map_;
    vector<vector<int>> m_;
    vector<vector<int>> m_closure_;
};

template <class T>
void print(T f) {
    cout << f << endl;
}
template <class T>
void print(const vector<T> &v) {
    for (auto &x : v)
        cout << x << " ";
    cout << endl;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    print(make_unique<WebsiteRank>()->countVotes({"C A B"}, "C"));
    print(make_unique<WebsiteRank>()->countVotes(
            {"A B C D", "B C D", "C D"}, "A"));
    print(make_unique<WebsiteRank>()->countVotes(
            {"A B C D E F", "B A", "C B", "D B"}, "A"));
    return 0;
}
