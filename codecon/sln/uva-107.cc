#include <algorithm>
#include <map>
#include <stdio.h>
#include <string>
#include <vector>

#define ARRAY_SIZEOF(a) (sizeof(a) / sizeof(a[0]))
#define INF 0x7fffffff

using namespace std;

int findPowerOf2(int n) {
    int p = 0;
    for (; n % 2 == 0; n /= 2) {
        p++;
    }
    return p;
}

int power(int x, int n) {
    if (n == 0) {
        return 1;
    }
    int p = 1;
    for (; n; n >>= 1) {
        int overflow = INF / x;
        if (n & 1) {
            if (p > overflow) {
                return INF;
            }
            p *= x;
        }
        if (x > overflow && n > 1) {
            return INF;
        }
        x *= x;
    }
    return p;
}

int findLog(int x, int p) {
    if (p == 0) {
        return 1;
    }
    int a = 1, b = x;
    while (a < b) {
        int m = a + (b - a) / 2;
        int m_p = power(m, p);
        if (x > m_p) {
            a = m + 1;
        } else {
            b = m;
        }
    }
    return a;
}

bool solve() {
    int i, j, a, b, t, n;

    scanf("%d%d", &a, &b);
    if (a == 0 && b == 0) {
        return false;
    }

    if (b > a) {
        t = a;
        a = b;
        b = t;
    }
    int p2 = findPowerOf2(a % 2 ? b : a);

    // p2 is in [1,31] so just for is OK.
    for (i = 0; i <= p2; i++) {
        if (i != 0 && p2 % i != 0) {
            continue;
        }
        n = findLog(b, i);
        int npowi = power(n, i);
        if (npowi == b && power(n + 1, i) == a) {
            break;
        }
    }

    int num = 0, height_sum = 0, l = 1, h = a;
    for (j = 0; j < i; j++) {
        height_sum += h * l;
        num += l;
        l *= n;
        h /= (n + 1);
    }
    height_sum += h * l;

    printf("%d %d\n", num, height_sum);
    return true;
}

int main() {
#ifndef ONLINE_JUDGE
    freopen("in", "r", stdin);
#endif
    while (solve()) {
    }
    return 0;
}
