#include <algorithm>
#include <cstdio>
#include <string>
#include <utility>
#include <vector>

using namespace std;

void solve() {
    static char s[2 * 100'000 + 4];
    int n, k;
    scanf("%d%d%s", &n, &k, s);
    vector<int> stack;
    int len = 0;
    for (int i = 0; i < n && len < k; i++) {
        switch (s[i]) {
        case '(':
            stack.push_back(i);
            break;
        case ')':
            stack.pop_back();
            len += 2;
            break;
        }
    }
    int stackj = 0;
    int j1 = 0;
    int j2 = 0;
    while (j2 < len) {
        if (stackj < stack.size() && stack[stackj] == j1) {
            stackj++;
        } else {
            s[j2++] = s[j1];
        }
        j1++;
    }
    s[j2] = 0;
    printf("%s\n", s);
}

int main(int argc, char **argv) {
    solve();
    return 0;
}
