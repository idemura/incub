'use strict';

function print() {
  console.log.apply(null, arguments);
}

function Scanner(text) {
  this.splits = text.split(/\s+/);
  this.p = 0;
}

Scanner.prototype.getInt = function() {
  if (this.p < this.splits.length) {
    return parseInt(this.splits[this.p++], 10);
  }
}

Scanner.prototype.getStr = function() {
  if (this.p < this.splits.length) {
    return this.splits[this.p++];
  }
}

Scanner.prototype.eof = function() {
  return this.p == this.splits.length;
}

function input(callback) {
  var fs = require('fs');
  fs.readFile('in', {encoding: 'utf8'}, function(err, data) {
    if (err) {
      process.exit(1);
    }
    // split on spaces, save that vector. have methods get int, string.
    callback(new Scanner(data));
  });
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

function binaryString(n, width) {
  var s = n.toString(2);
  while (s.length < width) {
    s = '0' + s;
  }
  return s;
}

// Eratosphenes sieve.
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

// (~k + 1) is the same as -k and value is least bin that is != 0.
function BIT(n) {
  var a = [];
  a[n] = undefined;
  for (var i = 0; i <= n; i++) {
    a[i] = 0;
  }
  this.a = a;
  this.length = n;
}

BIT.prototype = {
  update: function(i, d) {
    while (i < this.a.length) {
      this.a[i] += d;
      i += i & -i;
    }
  },

  sum: function(i) {
    var sum = 0;
    while (i) {
      sum += this.a[i];
      i -= i & -i;
    }
    return sum;
  },

  // Although better method exists, this is OK. Handles border case.
  get: function(i) {
    var si = this.sum(i);
    if (i > 1) {
      si -= this.sum(i - 1);
    }
    return si;
  },

  toArray: function() {
    var a = [];
    for (var i = 1, l = this.a.length; i < l; i++) {
      a[i - 1] = this.sum(i);
    }
    return a;
  }
};

// Generates combinations by `k` of array `as` and calls `callback` on every.
function comb(as, k0, callback) {
  var n = as.length;
  var set = [];
  function rec(i, k) {
    if (k === 0) {
      callback(set);
      return;
    }
    for (var j = i; j + k <= n; j++) {
      set[k0 - k] = as[j];
      rec(j + 1, k - 1);
    }
  }

  rec(0, k0);
}

function unique(as) {
  var us = [];
  as.sort();  // OK to sort lexicographically.
  for (var i = 1; i < as.length; i++) {
    if (as[i - 1] != as[i]) {
      us.push(as[i]);
    }
  }
  return us;
}