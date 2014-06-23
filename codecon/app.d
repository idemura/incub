import std.algorithm, std.conv, std.json, std.stdio;

T getCoverageOf(T)(T vs, T[] m)
{
  auto r = vs;
  size_t k = 0;
  for (auto i = vs; i != 0; i >>>= 1) {
    if (i & 1) {
      r |= m[k];
    }
    k++;
  }
  return r;
}

int countSetBits(ulong n)
{
  auto c = 0;
  for (; n; n = n & (n - 1)) {
    c++;
  }
  return c;
}

unittest {

}

T findMinimalCover(T)(T[] m)
{
  immutable n = m.length;
  foreach (i; 0 .. 2 ^^ n) {
    auto coverage = getCoverageOf(i, m);
    if (coverage == np - 1) {
      return gray_code;
    }
  }
  return null;
}

uint[] combinations(int m, int n)
{
  static void recStep(int m, int n, uint s, int b, ref uint[] acc)
  {
    if (m > n) {
      return;
    }
    if (m == 0) {
      acc ~= s;
    } else {
      recStep(m - 1, n - 1, s | b, b << 1, acc);
      recStep(m, n - 1, s, b << 1, acc);
    }
  }

  if (m > n) {
    return null;
  }
  auto acc = new uint[](0);
  recStep(m, n, 0, 1, acc);
  return acc;
}

void printSets(T)(int n, T[] c)
{
  auto format = "%0" ~ to!string(n) ~ "b";
  foreach (s; c) {
    writefln(format, s);
  }
  // writeln();
}

void testCombinations()
{
  immutable n = 4;
  foreach (i; 0 .. n + 1) {
    writefln("C %s %s:", i, n);
    printSets(n, combinations(i, n));
  }
  writefln("");
}

size_t getDecliningSequence(T)(T[] a)
{
  size_t i = 1;
  for (; i < a.length && a[i - 1] < a[i]; i++) {
    // Empty.
  }
  return i;
}

uint[] permutations(int n)
{
  static bool step(uint[] p)
  {
    auto i = getDecliningSequence(p);
    if (i == p.length) {
      return false;
    }
    // a[i] > a[i - 1].
    // Reverse and find the place where to put a[i] so we don't break the order.
    reverse(p[0 .. i]);
    // We can do it binary O(log N) time, but do linearly for simplicity.
    typeof(i) j = 0;
    for (; j < i; j++) {
      if (p[j] > p[i]) break;
    }
    if (j == i) {
      j = i - 1;
    }
    return true;
  }

  auto p = new uint[](n);
  foreach (i, ref pi; p) {
    pi = i;
  }

  auto acc = new uint[](0);
  do {
    acc ~= p;
  } while (step(p));
  return acc;
}

void main(string[] args)
{
  testCombinations();

  //auto gc = grayCodes(3);
  //foreach (c; gc) {
  //  writefln("%03b", c);
  //}

  try {
    auto f = File("in", "rt");
    int n;
    f.readf(" %d", &n);
    writefln("%s", ulong.sizeof);
    if (n > 8 * ulong.sizeof) {
      throw new Exception("Too many vertices");
    }
    auto m = new ulong[](n);
    foreach (i; 0 .. n) {
      int a, b;
      f.readf(" %d %d", &a, &b);
      m[a] |= 1ul << b;
      m[b] |= 1ul << a;
    }
    findMinimalCover(m);
  } catch (Exception e) {
    writefln("Exception: %s", e.msg);
  }
}
