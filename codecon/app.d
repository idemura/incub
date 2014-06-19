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

void main(string[] args)
{
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
