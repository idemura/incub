#include <iostream>
#include <algorithm>
#include <set>

using namespace std;

typedef long long int lli;

lli sum(set<int>& s, int l, int r)
{
    auto itl = s.lower_bound(l);
    auto itr = s.upper_bound(r);
    if (itl == itr) {
        return 0;
    }

    lli sum = 0;
    lli d = distance(itl, itr);
    for (--itr; d > 1; ++itl, --itr) {
        sum += (d - 1) * (*itr - *itl);
        d -= 2;
    }
    return sum;
}

int main()
{
    int n = 0;
    cin >> n;
    vector<int> xs(n);
    set<int> level;
    for (int i = 0; i < n; ++i) {
        cin >> xs[i];
        level.insert(xs[i]);
    }
    int m = 0;
    cin >> m;
    for (int i = 0; i < m; ++i) {
        int type = 0, p1 = 0, p2 = 0;
        cin >> type >> p1 >> p2;
        if (type == 1) {
            int j = p1 - 1;
            level.erase(xs[j]);
            level.insert(xs[j] += p2);
        } else {
            cout << sum(level, p1, p2) << endl;
        }
    }
    return 0;
}
