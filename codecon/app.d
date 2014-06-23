import std.algorithm, std.conv, std.json, std.stdio;

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

size_t getDecreasingSeqLength(T)(T[] a)
{
  size_t i = 1;
  for (; i < a.length && a[i - 1] > a[i]; i++) {
    // Empty.
  }
  return i;
}

int[][] permutations(int n)
{
  static bool step(int[] p)
  {
    auto i = getDecreasingSeqLength(p);
    if (i == p.length) {
      return false;
    }
    // a[i] > a[i - 1].
    // Reverse and find the place where to put a[i] so we don't break the order.
    if (i > 1) {
      reverse(p[0 .. i]);
    }
    // We can do it binary O(log N) time, but do linearly for simplicity.
    typeof(i) j = 0;
    for (; j < i; j++) {
      if (p[j] > p[i]) break;
    }
    // `j` is always greater 0 (because p[0] is min always?).
    swap(p[i], p[j - 1]);
    return true;
  }

  auto p = new int[](n);
  foreach (i, ref pi; p) {
    pi = cast(int)i;
  }

  auto acc = new int[][](0);
  do {
    acc ~= p.dup;
  } while (step(p));
  return acc;
}

void testPermutations()
{
  writeln("Permutation of 3:");
  writeln(permutations(3));
}

void main(string[] args)
{
  // testCombinations();
  testPermutations();

  //auto gc = grayCodes(3);
  //foreach (c; gc) {
  //  writefln("%03b", c);
  //}

  // try {
  //   auto f = File("in", "rt");
  //   int n;
  //   f.readf(" %d", &n);
  //   writefln("%d", n);
  // } catch (Exception e) {
  //   writefln("Exception: %s", e.msg);
  // }
}
