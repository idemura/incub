#include "base.h"

int gcd(int a, int b) {
  while (b != 0) {
    int t = a % b;
    a = b;
    b = t;
  }
  return a;
}

int ext_euclid(int m, int n, int* km, int *kn) {
  int x = m, y = n;
  int xu = 1, xv = 0;
  int yu = 0, yv = 1;
  // Maintain the following predicate:
  //  x = xu * m + xv * n;
  //  y = yu * m + yv * n;
  // Each step we have y[n+1] = x[n] - q * y[n], hence formulas.
  int r = x % y;
  while (r) {
    int t;
    int q = x / y;
    x = y;
    y = r;
    t = xu;
    xu = yu;
    yu = t - q * yu;
    t = xv;
    xv = yv;
    yv = t - q * yv;
    r = x % y;
  }
  *km = yu;
  *kn = yv;
  return y;
}

void test(int m, int n, int gcd_expected) {
  int km, kn;
  int gcd = ext_euclid(m, n, &km, &kn);
  CHECK(gcd == gcd_expected);
  CHECK(m % gcd == 0);
  CHECK(n % gcd == 0);
  CHECK(gcd == km * m + kn * n);
}

int main(int argc, char **argv) {
  test(4, 6, 2);
  test(6, 4, 2);
  test(9, 4, 1);
  test(3, 7, 1);
  test(12, 15, 3);
  test(10, 15, 5);
  cout << "TESTS PASSED." << endl;
  return 0;
}
