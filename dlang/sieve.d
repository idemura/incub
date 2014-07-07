import std.algorithm, std.conv, std.stdio;

// Linear time sieve algorithm. Output array will contain for each the minimal
// divisor for each integer [0..n] (0 for index 0 and 1).
int[] linearSieve(int n) {
  auto a = new int[](n + 1);
  int[] primes;
  for (int i = 2; i < a.length; i++) {
    if (a[i] == 0) {
      a[i] = i;
      primes ~= i;
    }
    for (int j = 0; j < primes.length
         && primes[j] <= a[i]  // Prime is less or equal then minimal divisor.
         && i * primes[j] < a.length;  // Bounds check.
         j++) {
      // Test everything is written once (algorithm guarantees).
      assert(a[i * primes[j]] == 0);
      a[i * primes[j]] = primes[j];
    }
  }
  return a;
}

void main(string[] args) {
  auto s = linearSieve(18);
  writefln("%s", s);
}
