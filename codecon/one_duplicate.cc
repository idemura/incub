#include <algorithm>
#include <iostream>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "log.hpp"

using std::cout;
using std::endl;

int findDuplicate(std::vector<int> v) {
    for (int i = 0; i < v.size();) {
        if (v[i] == i + 1) {
            i++;
        } else {
            if (v[i] == v[v[i] - 1]) {
                return v[i];
            } else {
                std::swap(v[i], v[v[i] - 1]);
            }
        }
    }
    return -1;
}

int main() {
    cout << findDuplicate(std::vector<int>{1, 2, 4, 3, 1}) << endl;
    cout << findDuplicate(std::vector<int>{2, 1, 4, 1, 3}) << endl;
    cout << findDuplicate(std::vector<int>{2, 1, 5, 1, 3}) << endl;
    return 0;
}
