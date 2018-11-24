#include <algorithm>
#include <chrono>
#include <cmath> // Overloads for abs.
#include <cstdlib>
#include <cstring>
#include <functional>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <memory>
#include <random>
#include <set>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

using namespace std;

using i64 = long long int;
using i32 = int;

constexpr char kEol[] = "\n";
constexpr int INF = 0x7fffffff;
constexpr int MOD = 1000000007;

template <class T>
unique_ptr<T> wrap_unique(T *p) {
    return unique_ptr<T>(p);
}

i64 gcd(i64 a, i64 b) {
    while (b != 0) {
        i64 t = a % b;
        a = b;
        b = t;
    }
    return a;
}

int main(int argc, char **argv) {
    ios_base::sync_with_stdio(false);
    int n;
    cin >> n;
    i64 g;
    cin >> g;
    for (int i = 1; i < n; i++) {
        i64 t;
        cin >> t;
        g = gcd(g, t);
    }
    cout << g << endl;
    return 0;
}
