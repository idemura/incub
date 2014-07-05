import std.algorithm, std.conv, std.random, std.stdio;

int modPow(int a, int p, int m)
{
  long r = 1;
  long b = a;
  for (; p > 0; p >>>= 1) {
    if (p & 1) {
      r = (r * b) % m;
    }
    b = (b * b) % m;
  }
  return cast(int)r;
}

bool isPrimeFermat(int n, int testTimes)
{
  static auto rng = MinstdRand(1);
  foreach (i; 0..testTimes) {
    if (modPow(rng.front, n - 1, n) != 1) {
      return false;
    }
    rng.popFront();
  }
  return true;
}

void testFermat(int n)
{
  writefln("%s is %s", n, isPrimeFermat(n, 2)? "probably prime": "composite");
}

void main(string[] args)
{
  foreach (n; [6, 11, 17, 18, 24, 25, 34, 43, 51, 101]) {
    testFermat(n);
  }
}
