#include "base.h"

// Given an array of integers. We know that one integer occurs more than n/2
// times. Find it.
// Idea is that consecutive integers of the number we looking for happens more
// than any others, even all together.
// For simplicity, we assume integers are positive.
int find_dominant(const vector<int> &a) {
  int x = 0;
  int l = 0;
  int x_max = 0;
  int l_max = 0;
  for (int i = 1; i < a.size(); i++) {
    if (a[i - 1] == a[i]) {
      l++;
      x = a[i];
    } else if (l > 0) {
      if (l >= l_max) {
        l_max = l - l_max;
        x_max = x;
      } else {
        l_max -= l;
      }
      x = 0;
      l = 0;
    }
  }
  if (l >= l_max) x_max = x;
  return x_max;
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

