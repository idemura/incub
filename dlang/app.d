import std.algorithm, std.conv, std.stdio;

T upperPowOf2(T)(T n)
{
  T p = 1;
  for (; p < n; p *= 2) {
  }
  return p;
}

int[] buildRmqTree(int[] xs)
{
  auto pot_length = upperPowOf2(xs.length);
  auto a = new int[](2 * pot_length - 1);
  auto i = a.length;
  if (pot_length != xs.length) {
    for (auto j = pot_length; j != xs.length; j--) {
      a[--i] = int.max;
    }
  }
  for (auto j = xs.length; j > 0; j--) {
    a[--i] = xs[j - 1];
  }
  for (; i > 0; i--) {
    a[i - 1] = min(a[2 * (i - 1) + 1], a[2 * (i - 1) + 2]);
  }
  return a;
}

int rangeMin(int[] ns, size_t i, size_t j)
{
  int stepRec(int k, size_t m, size_t n)
  {
    if (n <= i || m >= j) {
      return int.max;
    }
    writefln("k %s m %s n %s", k, m, n);
    if (i <= m && n <= j) {
      writefln("  return ns[k] %s", ns[k]);
      return ns[k];
    }
    auto min1 = stepRec(2 * k + 1, m, (m + n) / 2);
    auto min2 = stepRec(2 * k + 2, (m + n) / 2, n);
    return min(min1, min2);
  }
  writefln("i %s j %s", i, j);
  return stepRec(0, 0, 7);
}

int rangeMinLinear(int[] a, size_t i, size_t j)
{
  auto m = a[i++];
  for (; i < j; i++) {
    m = min(m, a[i]);
  }
  return m;
}

void main(string[] args)
{
  auto xs = [6, 2, 7, 1, 2, 4, 3];
  auto a = buildRmqTree(xs);
  for (size_t s = 1; s <= xs.length; s++) {
    for (size_t i = 0; i + s <= xs.length; i++) {
      auto m1 = rangeMinLinear(xs, i, i + s);
      auto m2 = rangeMin(a, i, i + s);
      if (m1 != m2) {
        writefln("FAIL on %s, %s: %s", i, i + s, xs[i .. i + s]);
        writefln("  ACTUAL: %s", m2);
        writefln("  EXPECTED: %s", m1);
      }
    }
  }
}
