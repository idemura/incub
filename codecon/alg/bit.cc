#include "base.h"

// Indices are 1..n (inclusive).
template<class Num>
class BIT {
public:
  explicit BIT(int n): v_(n + 1), a_(n + 1) {}
  size_t size() const { return a_.size() - 1; }

  void add(int i, Num x) {
    a_[i] += x;
    while (i < v_.size()) {
      v_[i] += x;
      i += i & (-i);
    }
  }

  void set(int i, Num x) {
    add(i, x - a_[i]);
  }

  Num get(int i) const { return a_[i]; }

  // Sum of [a..b], inclusive.
  Num sum(int a, int b) const {
    return sum(b) - sum(a - 1);
  }

  // Sum of [0..i], inclusive.
  Num sum(int i) const {
    auto a = Num();
    while (i != 0) {
      a += v_[i];
      i -= i & (-i);
    }
    return a;
  }

private:
  vector<Num> v_, a_;
};

void test1() {
  BIT<double> bit(11);
  bit.add(1, 1);
  CHECK(bit.sum(1, 1) == 1);
  CHECK(bit.sum(1, 2) == 1);
  CHECK(bit.sum(1, 11) == 1);
  bit.add(3, 2);
  CHECK(bit.sum(1, 1) == 1);
  CHECK(bit.sum(1, 2) == 1);
  CHECK(bit.sum(1, 3) == 3);
  CHECK(bit.sum(1, 4) == 3);
  CHECK(bit.sum(1, 5) == 3);
  CHECK(bit.sum(1, 11) == 3);
  bit.add(1, 1);
  CHECK(bit.sum(1, 1) == 2);
  CHECK(bit.sum(1, 2) == 2);
  CHECK(bit.sum(1, 3) == 4);
  CHECK(bit.sum(1, 4) == 4);
  bit.set(1, 0);
  CHECK(bit.sum(1, 1) == 0);
  CHECK(bit.sum(1, 2) == 0);
  CHECK(bit.sum(1, 3) == 2);
  CHECK(bit.sum(1, 4) == 2);
  CHECK(bit.get(1) == 0);
  CHECK(bit.get(2) == 0);
  CHECK(bit.get(3) == 2);
  bit.set(5, 5);
  bit.set(6, 3);
  bit.add(8, 2);
  bit.add(1, 1);
  bit.add(11, -1.5);
  // 1:1, 3:2, 5:5, 6:3, 8:2, 11:-1
  // Test `sum`:
  CHECK(bit.sum(1) == 1);
  CHECK(bit.sum(2) == 1);
  CHECK(bit.sum(3) == 3);
  CHECK(bit.sum(4) == 3);
  CHECK(bit.sum(5) == 8);
  CHECK(bit.sum(6) == 11);
  CHECK(bit.sum(7) == 11);
  CHECK(bit.sum(8) == 13);
  CHECK(bit.sum(9) == 13);
  CHECK(bit.sum(10) == 13);
  CHECK(bit.sum(11) == 11.5);
  CHECK(bit.sum(3, 5) == 7);
  CHECK(bit.sum(4, 6) == 8);
  CHECK(bit.sum(4, 7) == 8);
  CHECK(bit.sum(8, 10) == 2);
  CHECK(bit.sum(8, 11) == 0.5);
  // Test `get`:
  CHECK(bit.get(11) == -1.5);
  CHECK(bit.get(8) == 2);
  CHECK(bit.get(10) == 0);
  CHECK(bit.get(9) == 0);
  cout << "TEST1 passed" << endl;
}

int main(int argc, char **argv) {
  ios_base::sync_with_stdio(false);
  test1();
  cout << "TESTS PASSED." << endl;
  return 0;
}
