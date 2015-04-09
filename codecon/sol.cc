#include <algorithm>
#include <functional>
#include <iostream>
#include <list>
#include <map>
#include <unordered_map>
#include <string>
#include <queue>
#include <vector>
#include <memory>
#include <sstream>
#include <utility>
#include <math.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ARRAY_SIZEOF(A) (sizeof(A) / sizeof(A[0]))
#define NON_COPYABLE(C) \
    C(const C&); \
    C& operator=(const C&);

using namespace std;

typedef long long int i64;

constexpr int INF = 0x7fffffff;
constexpr int DIM = 200000;

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  for (int i = 0; i < 100; i++) {
    auto n = 100 - 3 * i;
    if (n > 0 && n % 2 == 0) {
      cout << "3*" << i << "+2*" << (n / 2) << " = 100\n";
    }
  }
  return 0;
}
