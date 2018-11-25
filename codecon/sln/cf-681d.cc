#include <algorithm>
#include <cmath> // Overloads for abs.
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <memory>
#include <random>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using u64 = unsigned i64;
using i32 = int;
using u32 = unsigned i32;

using AdjL = vector<vector<int>>;

// Every node n in a subtree of a @node with node == node->to should have
// n->to == node.
bool dfs(
        const AdjL &al,
        const vector<int> &ancestor,
        int v,
        int root,
        vector<int> *res) {
    if (ancestor[v] == v) root = v;
    if (ancestor[v] != root) {
        return false;
    }
    for (auto u : al[v]) {
        if (!dfs(al, ancestor, u, root, res)) return false;
    }
    // Put them in list in post order.
    if (ancestor[v] == v) {
        res->push_back(v + 1);
    }
    return true;
}

int main(int argc, char **argv) {
    int n, m, p, q;
    cin >> n >> m;
    AdjL al(n);
    vector<int> ancestor(n), has_parent(n);
    for (int i = 0; i < m; i++) {
        cin >> p >> q;
        al[p - 1].push_back(q - 1);
        has_parent[q - 1] = 1;
    }
    for (int i = 0; i < n; i++) {
        cin >> p;
        ancestor[i] = p - 1;
    }
    vector<int> res;
    for (int i = 0; i < n; i++) {
        if (has_parent[i]) continue;
        if (!dfs(al, ancestor, i, -1, &res)) {
            res.clear(); // Clear list is never a valid.
            break;
        }
    }
    if (res.empty()) {
        cout << "-1\n";
    } else {
        cout << res.size() << "\n";
        for (auto k : res)
            cout << k << "\n";
    }
    return 0;
}
