#include "base.h"

// Given an array of integers. We know that one integer occurs more than n/2
// times. Find this "dominant" element.
// Idea is that consecutive integers of the number we looking for happens more
// than any others, even all together.
// For simplicity, we assume integers are positive.
int find_dominant(const vector<int> &a) {
  int x = 0;
  int l = 0;
  for (int i = 1; i < a.size(); i++) {
    if (x == a[i]) {
      l++;
    } else {
      l--;
      if (l == 0) x = a[i];
    }
  }
  return x;
}

int main(int argc, char **argv) {
  // ios_base::sync_with_stdio(false);
  CHECK(1 == find_dominant({1, 1, 2, 2, 2, 2, 1, 1, 1, 2, 1, 1}));
  CHECK(1 == find_dominant({1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1, 1}));
  CHECK(1 == find_dominant({1, 1, 2, 1, 2, 1, 2, 1, 2, 1, 2, 1}));
  CHECK(1 == find_dominant({1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 1, 3}));
  CHECK(1 == find_dominant({2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1}));
  CHECK(1 == find_dominant({1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2}));
  cout << "TESTS PASSED." << endl;
  return 0;
}

