#include <algorithm>
#include <vector>
#include <map>
#include <utility>
#include <cstdio>
#include <cstring>
#include <cstdlib>
#include <cassert>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff
#define MOD 1000000007

using namespace std;

#define DIM 5001

int rems[DIM];

int main()
{
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    int i;

    int a, b;
    scanf("%d%d", &a, &b);
    if (a >= b) {
        printf("%d.", a / b);
        a %= b;
    } else {
        printf("0.");
    }

    vector<int> dds;
    while (rems[a] == 0) {
        rems[a] = dds.size() + 1;
        int d = 10 * a / b;
        int x = 10 * a % b;
        dds.push_back(d);
        a = x;
    }

    const int cycle = rems[a] - 1;
    for (i = 0; i < cycle; i++) {
        printf("%d", dds[i]);
    }
    printf("(");
    for (; i < dds.size(); i++) {
        printf("%d", dds[i]);
    }
    printf(")\n");
    return 0;
}
