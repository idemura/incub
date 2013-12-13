// console.log("GCD", gcd(12, 8));

function gcd(a, b) {
  var t;
  if (a < b) {
    t = a; a = b; b = t;
  }
  if (b === 0) {
    return;
  }
  while (b) {
    t = a % b;
    a = b;
    b = t;
  }
  return a;
}

var gray = {
  encode: function (n) {
    return n ^ (n >>> 1);
  },

  decode: function (g) {
    for (var n = 0; g !== 0; g >>>= 1) {
      n ^= g;
    }
    return n;
  }
};

function binStr(n, width) {
  var s = n.toString(2);
  while (s.length < width) {
    s = "0" + s;
  }
  return s;
}

// var WIDTH = 3;
// for (var i = 0; i < 8; i++) {
//  var g = gray.encode(i);
//  console.log(binStr(i, WIDTH), binStr(g, WIDTH),
//              "decode:", binStr(gray.decode(g), WIDTH));
// }

function primes(n) {
  var a = new Array(n + 1);
  var p = 2, pMax = Math.floor(Math.sqrt(n));
  for (; p <= pMax; p++) {
    if (!a[p]) {
      for (var i = p * p; i <= n; i += p) {
        a[i] = true;
      }
    }
  }
  var out = [];
  for (p = 2; p <= n; p++) {
    if (!a[p])
      out.push(p);
  }
  return out;
}

function isPrime(a) {
  if (a % 2 === 0 || a % 3 === 0) {
    return false;
  }
  var d = 5, s = 2;
  while (d * d <= a) {
    if (a % d === 0)
      return false;
    d += s;
    s = 6 - s;
  }
  return true;
}

// console.log(isPrime(12));
// console.log(isPrime(23));
// console.log(isPrime(982451653));  // This is prime.
// console.log(primes(16));
