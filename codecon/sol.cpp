#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

typedef long long int lli;

int bin_search(vector<int>& xs_sorted, int k)
{
    int j0 = 0;
    int j1 = xs_sorted.size();
    while (j0 < j1) {
        int m = j0 + (j1 - j0) / 2;
        if (k <= xs_sorted[m]) {
            j1 = m;
        } else {
            j0 = m + 1;
        }
    }
    return j0;
}

lli sum(vector<int>& xs, int l, int r)
{
    int j0 = bin_search(xs, l);
    int j1 = bin_search(xs, r);
    if (j0 == j1) {
        return 0;
    }
    if (j1 == xs.size() || r != xs[j1]) {
        j1--;
    }

    lli sum = 0;
    for (; j0 < j1; ++j0, --j1) {
        sum += (lli(j1 - j0)) * (xs[j1] - xs[j0]);
    }
    return sum;
}

void move(vector<int>& xs, int v, int d)
{
    int i = bin_search(xs, v);
    int n = xs.size();
    int new_k = xs[i] + d;
    if (d < 0) {
        for (; i > 0 && xs[i - 1] > new_k; --i) {
            xs[i] = xs[i - 1];
        }
    } else {
        for (; i + 1 < n && xs[i + 1] < new_k; ++i) {
            xs[i] = xs[i + 1];
        }
    }
    xs[i] = new_k;
}

int main()
{
    int n = 0;
    cin >> n;

    vector<int> xs(n);
    for (int i = 0; i < n; ++i) {
        cin >> xs[i];
    }
    vector<int> xs_sort(xs);
    sort(xs_sort.begin(), xs_sort.end());

    int m = 0;
    cin >> m;
    for (int i = 0; i < m; ++i) {
        int type = 0, p1 = 0, p2 = 0;
        cin >> type >> p1 >> p2;
        if (type == 1) {
            int j = p1 - 1;
            int v = xs[j];
            xs[j] += p2;
            move(xs_sort, v, p2);
        } else {
            cout << sum(xs_sort, p1, p2) << endl;
        }
    }
    return 0;
}
