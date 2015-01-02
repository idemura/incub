import std.algorithm, std.conv, std.stdio;

T upperPowOf2(T)(T n) {
  T p = 1;
  for (; p < n; p *= 2) {
  }
  return p;
}

int[] buildRmqTree(int[] v) {
  auto pot_length = upperPowOf2(v.length);
  auto a = new int[](2 * pot_length - 1);
  auto i = a.length;
  if (pot_length != v.length) {
    for (auto j = pot_length; j != v.length; j--) {
      a[--i] = int.max;
    }
  }
  for (auto j = v.length; j > 0; j--) {
    a[--i] = v[j - 1];
  }
  for (; i > 0; i--) {
    a[i - 1] = min(a[2 * (i - 1) + 1], a[2 * (i - 1) + 2]);
  }
  return a;
}

int rangeMin(int[] t, size_t i0, size_t i1) {
  int rec(int k, size_t j0, size_t j1) {
    if (j1 <= i0 || i1 <= j0) {
      return int.max;
    }
    if (i0 <= j0 && j1 <= i1) {
      return t[k];
    }
    auto min1 = rec(2 * k + 1, j0, (j0 + j1) / 2);
    auto min2 = rec(2 * k + 2, (j0 + j1) / 2, j1);
    return min(min1, min2);
  }
  return rec(0, 0, (t.length + 1) / 2);
}

int rangeMinLinear(int[] a, size_t i, size_t j) {
  auto m = a[i++];
  for (; i < j; i++) {
    m = min(m, a[i]);
  }
  return m;
}

void testRangeMin(int[] v, int[] t, size_t i0, size_t i1) {
  auto m1 = rangeMinLinear(v, i0, i1);
  auto m2 = rangeMin(t, i0, i1);
  if (m1 != m2) {
    writefln("FAIL on %s, %s: %s", i0, i1, v[i0 .. i1]);
    writefln("  ACTUAL: %s", m2);
    writefln("  EXPECTED: %s", m1);
  }
}

void main(string[] args) {
  auto v = [6, 2, 7, 1, 2, 4, 3];
  auto t = buildRmqTree(v);
  for (size_t s = 1; s <= v.length; s++) {
    for (size_t i = 0; i + s <= v.length; i++) {
      testRangeMin(v, t, i, i + s);
    }
  }
}
