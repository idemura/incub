#include <algorithm>
#include <array>
#include <map>
#include <stdio.h>
#include <string>
#include <unordered_map>
#include <vector>

using namespace std;

using i64 = long long int;

namespace {
array<int, 1'000'001> cc;
}

int main() {
    int n = 0, a = 0;
    scanf("%d%d", &n, &a);
    int inc_counter = 0;
    for (int i = 0; i < n; i++) {
        int c = 0;
        scanf("%d", &c);
        if (c == a) {
            inc_counter++;
        } else {
            if (cc[c] >= inc_counter) {
                cc[c]++;
            }
        }
    }
    int b = -1;
    for (int i = 1; i < cc.size(); i++) {
        if (cc[i] >= inc_counter && a != i) {
            b = i;
            break;
        }
    }
    printf("%d\n", b);
    return 0;
}
