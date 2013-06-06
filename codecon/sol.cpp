#include <iostream>
#include <algorithm>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define ZERO(p, n)      memset(p, 0, n)

using namespace std;

typedef long long int lli;

struct intp {
    int x, c;
};

int main(void)
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i, j, k, n;
    cin >> n;

    vector<intp> ns(n);
    j = 0;
    for (i = 0; i < n; i++) {
        int d;
        cin >> d;
        if (j == 0 || ns[j].x != d) {
            ns[j].x = d;
            ns[j].c = 1;
            j++;
        } else {
            ns[j].c++;
        }
    }

    int last = 0, sum = 0;
    for (i = 0; i < ns.size(); i++) {
        if (ns[i].c & 1) {
            last = ns[i].x;
            sum += last - ns[i].x;
        }
        int c = ns[i].c >> 1;
        j = i + 1;
        k = 1;
        for (; c != 0; c >>= 1) {
            for (; ns
        }
    }

    return 0;
}
