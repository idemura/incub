#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

// Because all unique, cycles will meet on the beginning, not
// middle (otherwise two element will have same permutation
// value, which conflicts with uniquity).
int main() {
    for (;;) {
        int n = 0;
        cin >> n;
        if (!n) break;
        vector<int> perm(n);
        for (int i = 0; i < n; i++) {
            int k = 0;
            cin >> k;
            perm[i] = k - 1;
        }
        vector<vector<int>> cycle(n);
        for (int i = 0; i < n; i++) {
            const auto a0 = perm[i];
            cycle[i].push_back(a0);
            for (auto ai = perm[a0]; ai != a0; ai = perm[ai]) {
                cycle[i].push_back(ai);
            }
        }
        for (;;) {
            int shuffle = 0;
            cin >> shuffle;
            if (!shuffle) break;
            shuffle--;
            cin.get(); // Skip space.
            string l;
            getline(cin, l);
            l.resize(n, ' '); // Fill with spaces till size of n.
            string r(l.size(), ' ');
            for (int i = 0; i < l.size(); i++) {
                auto d = cycle[i][shuffle % cycle[i].size()];
                r[d] = l[i];
            }
            cout << r << endl;
        }
        cout << endl;
    }
    return 0;
}
