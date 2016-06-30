#include "base.h"

struct SegmPt {
  int s = 0;
  int x = 0;
  int left_side = 0;
  SegmPt() = default;
  SegmPt(int s, int x, int left_side): s(s), x(x), left_side(left_side) {}
};

int main(int argc, char **argv) {
  int n = 0;
  cin>>n;
  vector<SegmPt> v(2 * n);
  vector<int> left(n);
  for (int i = 0; i < n; i++) {
    int l, r;
    cin>>l>>r;
    v[2 * i] = SegmPt(i, l, 1);
    v[2 * i + 1] = SegmPt(i, r, 0);
    left[i] = l;
  }
  sort(v.begin(), e.end(), [](const SegmPt &a, const SegmPt &b) {
    return a.x < b.x;
  });
  vector<int> ans(n);
  unordered_set<int> cs;
  for (const auto& s : v) {
    if (s.left_side) {
    } else {

    }
  }
  return 0;
}

