function log() {
  console.log.apply(null, arguments);
}

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

// (function() {
//   log('GCD', gcd(12, 8));
//   log('GCD', gcd(15, 14));
// }());

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
    s = '0' + s;
  }
  return s;
}

// (function() {
//   var WIDTH = 3;
//   for (var i = 0; i < 8; i++) {
//    var g = gray.encode(i);
//    log(binStr(i, WIDTH), binStr(g, WIDTH), 'decode:',
//        binStr(gray.decode(g), WIDTH));
//   }
// }());

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

// (function() {
//   log(isPrime(12));
//   log(isPrime(23));
//   log(isPrime(982451653));  // This is prime.
//   log(primes(16));
// }());

function binarySearch(a, x) {
  var i = 0, j = a.length;
  while (i < j) {
    var m = Math.floor((i + j) / 2);
    // For upper bound x <= a[m].
    if (x <= a[m]) {
      j = m;
    } else {
      i = m + 1;
    }
  }
  return i;
}

// (function() {
//   function check(x) {
//     var i = binarySearch(a, x);
//     log('x=' + x + ' @' + i + ' ' + a.slice(i - 1, i + 2));
//   }
//
//   var a = [1, 4, 6, 8, 8, 10, 11];
//   check(0);
//   check(5);
//   check(6);
//   check(8);
//   check(10);
// }());

// `a` is [[x]]
function project(a) {
  return a.map(function (x) { return x[0]; });
}

function isIntersect(s, segms) {
  var i = binarySearch(project(segms), s[0]);
  if (i == segms.length) {
    return s[0] < segms[segms.length - 1][1];
  }
  if (i > 0 && s[0] < segms[i - 1][1]) {
    return true;
  }
  return s[1] > segms[i][0];
}

(function() {
  function check(s, expected) {
    var intersect = isIntersect(s, segms);
    log('[' + s[0] + ', ' + s[1] + '] is ' + intersect);
    if (intersect != expected) {
      log('EXPECTED: ' + expected);
    }
  }

  var segms = [[3, 5], [10, 13], [16, 18]];
  check([1, 2], false);
  check([1, 3], false);
  check([1, 4], true);
  check([4, 6], true);
  check([6, 8], false);
  check([4, 12], true);
  check([9, 14], true);
  check([15, 18], true);
  check([17, 19], true);
  check([15, 19], true);
  check([19, 20], false);
}());
