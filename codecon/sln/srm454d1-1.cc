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

int normal(int x) {
    return (x & 15) + ((x >> 4) & 15) * 10 + ((x >> 8) & 15) * 100 +
            ((x >> 12) & 15) * 1000 + ((x >> 16) & 15) * 10000 +
            ((x >> 20) & 20) * 100000 + ((x >> 24) & 15) * 1000000;
}

inline int dec(int x, int &nz) {
    int y = 0;
    if (nz > 0) {
        y = 9;
        if (nz > 4) y |= 9 << 4;
        if (nz > 8) y |= 9 << 8;
        if (nz > 12) y |= 9 << 12;
        if (nz > 16) y |= 9 << 16;
        if (nz > 20) y |= 9 << 20;
        // nz max is 6.
        if ((x >> nz) != 1) {
            y |= ((x >> nz) - 1) << nz;
        }
        nz = 0;
    } else {
        y = x - 1;
        if ((y & 15) == 0) {
            nz = 0;
            for (int y1 = y; (y1 & 15) == 0; y1 >>= 4, nz++)
                ;
            nz <<= 2;
        }
    }
    return y;
}

class DoubleXor {
public:
    int calculate(int n) {
        int x = 0;
        int nz = -1;
        for (int k = 0, m = n; m > 0; k++, m /= 10) {
            x |= ((m % 10) << (k * 4));
            if (m % 10 != 0 && nz < 0) nz = k;
        }
        nz *= 4;
        int y = x;
        for (n--; n > 0; n--) {
            y = dec(y, nz);
            int bit_xor = x ^ y, t = 0, b;
            b = bit_xor & 15;
            t |= b > 10 ? b - 10 : b;
            b = (bit_xor >> 4) & 15;
            t |= (b > 10 ? b - 10 : b) << 4;
            b = (bit_xor >> 8) & 15;
            t |= (b > 10 ? b - 10 : b) << 8;
            b = (bit_xor >> 12) & 15;
            t |= (b > 10 ? b - 10 : b) << 12;
            b = (bit_xor >> 16) & 15;
            t |= (b > 10 ? b - 10 : b) << 16;
            b = (bit_xor >> 20) & 15;
            t |= (b > 10 ? b - 10 : b) << 20;
            b = (bit_xor >> 24) & 15;
            t |= (b > 10 ? b - 10 : b) << 24;
            x = t;
        }
        return normal(x);
    }
};

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    {
        DoubleXor sol;
        cout << sol.calculate(1) << endl;
    }
    {
        DoubleXor sol;
        cout << sol.calculate(2) << endl;
    }
    {
        DoubleXor sol;
        cout << sol.calculate(7) << endl;
    }
    {
        DoubleXor sol;
        cout << sol.calculate(10) << endl;
    }
    {
        DoubleXor sol;
        cout << sol.calculate(102) << endl;
    }
    return 0;
}
