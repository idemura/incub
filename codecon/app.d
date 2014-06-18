import std.algorithm, std.conv, std.json, std.stdio;

struct Pt2i {
  int x, y;
}

int[] grayCodes(int n)
{
  auto a_num = 2 ^^ n;
  auto a = new int[a_num];
  foreach (i; 0 .. a_num) {
    a[i] = i ^ (i >> 1);
  }
  return a;
}

int[] getCoord(alias name)(Pt2i[] points)
{
  auto c = new int[points.length];
  foreach (i, p; points) {
    c[i] = mixin("p." ~ name);
  }
  return c;
}

long solve1D(int[] cs)
{
  if (cs.length == 1) {
    return cs[0];
  }
  sort(cs);  // Actually, we need the median index.
  if (cs.length % 2) {
    return 1;
  } else {
    auto i = (cs.length - 1) / 2;
    return cs[i + 1] - cs[i] + 1;
  }
}

long solve(Pt2i[] points)
{
  return solve1D(getCoord!"x"(points)) * solve1D(getCoord!"y"(points));
}

void main(string[] args)
{
  auto gc = grayCodes(3);
  foreach (c; gc) {
    writefln("%03b", c);
  }
  try {
    auto f = File("in", "rt");
    int t;
    f.readf(" %d", &t);
    foreach (i; 0 .. t) {
      int n;
      f.readf(" %d", &n);
      auto points = new Pt2i[n];
      foreach (j, ref p; points) {
        f.readf(" %d %d", &p.x, &p.y);
      }
      writefln("%s", solve(points));
    }
    //writefln("a %s b %s", a, b);
  } catch (Exception e) {
    writefln("Exception: %s", e.msg);
  }
}
