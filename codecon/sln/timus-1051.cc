#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

int count_min(int m, int n) {
    if (n <= 2) {
        return (m + 1) / 2;
    }
    if (m <= 2) {
        return (n + 1) / 2;
    }
    if (m % 3 == 0 || n % 3 == 0) {
        return 2;
    }
    return 1;
}

int main() {
    int n = 0, m = 0;
    cin >> n >> m;
    cout << count_min(m, n) << endl;
    return 0;
}
