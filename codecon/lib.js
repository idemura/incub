'use strict';

module.exports.Scanner = Scanner;
module.exports.input = input;
module.exports.print = print;
module.exports.gcd = gcd;
module.exports.gray = gray;
module.exports.Heap = Heap;
module.exports.binaryString = binaryString;
module.exports.sieve = sieve;
module.exports.isPrime = isPrime;
module.exports.binarySearch = binarySearch;
module.exports.BIT = BIT;
module.exports.comb = comb;
module.exports.unique = unique;

function print() {
  console.log.apply(null, arguments);
}

function Scanner(text) {
  this._splits = text.split(/\s+/);
  this._p = 0;
}

Scanner.prototype.getInt = function() {
  if (this._p < this._splits.length) {
    return parseInt(this._splits[this._p++], 10);
  }
}

Scanner.prototype.getStr = function() {
  if (this._p < this._splits.length) {
    return this._splits[this._p++];
  }
}

Scanner.prototype.eof = function() {
  return this._p == this._splits.length;
}

function input(file, callback) {
  var fs = require('fs');
  fs.readFile(file, {encoding: 'utf8'}, function(err, data) {
    if (err) {
      process.exit(1);
    }
    // Split on spaces, save that vector. have methods get int, string.
    callback(new Scanner(data));
  });
}

function defaultKeyFn(key) {
  return key? key: function(x) { return x; };
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
function sieve(n) {
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
  this._a = a;
  this._length = n;
}

BIT.prototype.update = function(i, d) {
  while (i < this._a.length) {
    this._a[i] += d;
    i += i & -i;
  }
};

BIT.prototype.sum = function(i) {
  var sum = 0;
  while (i) {
    sum += this._a[i];
    i -= i & -i;
  }
  return sum;
};

// Although better method exists, this is OK. Handles border case.
BIT.prototype.get = function(i) {
  var si = this.sum(i);
  if (i > 1) {
    si -= this.sum(i - 1);
  }
  return si;
};

BIT.prototype.toArray = function() {
  var a = [];
  for (var i = 1, l = this._a.length; i < l; i++) {
    a[i - 1] = this.sum(i);
  }
  return a;
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

function unique(as, cmp) {
  var us = [];
  as.sort(cmp);  // OK to sort lexicographically.
  for (var i = 1; i < as.length; i++) {
    if (as[i - 1] != as[i]) {
      us.push(as[i]);
    }
  }
  return us;
}

function Heap(keyFn) {
  this._keyFn = defaultKeyFn(keyFn);
  this._h = [];
}

function parentIndex(i) {
  return (i - 1) >>> 1;
}

Heap.prototype.size = function() {
  return this._h.length;
};

Heap.prototype.heapify = function(i) {
  var keyFn = this._keyFn, h = this._h, j, imin = i, t;
  while (1) {
    j = 2 * i + 1;
    if (j < h.length && keyFn(h[j]) < keyFn(h[imin])) {
      imin = j;
    }
    j = 2 * i + 2;
    if (j < h.length && keyFn(h[j]) < keyFn(h[imin])) {
      imin = j;
    }
    if (i != imin) {
      t = h[imin];
      h[imin] = h[i];
      h[i] = t;
      i = imin;
    } else {
      break;
    }
  }
};

Heap.prototype.check = function() {
  var keyFn = this._keyFn, h = this._h, i, p;
  for (i = 1; i < h.length; i++) {
    p = parentIndex(i);
    if (keyFn(h[i]) > keyFn(h[p])) {
      console.log(h);
      console.log('Heap corrupted: ' + h[p] + ' and ' + h[i]);
    }
  }
};

Heap.prototype.build = function(as) {
  var i, j, imin, h;
  h = this._h = as.slice();
  if (h.length > 1) {
    for (i = parentIndex(h.length - 1); i >= 0; i--) {
      this.heapify(i);
    }
  }
};

Heap.prototype.insert = function(x) {
  var keyFn = this._keyFn, h = this._h, i, p, t;
  h.push(x);
  i = h.length - 1;
  for (; i > 0; i = p) {
    p = parentIndex(i);
    if (keyFn(h[i]) > keyFn(h[p])) {
      t = h[i];
      h[i] = h[p];
      h[p] = t;
    } else {
      break;
    }
  }
};

Heap.prototype.remove = function() {
  var h = this._h, v;
  v = h[0];
  h[0] = h.pop();
  this.heapify(0);
  return v;
};

// Assumes unique keys in the collection.
function Treap(keyFn) {
  this._keyFn = defaultKeyFn(keyFn);
  this._root = null;
}

function TreapNode(x) {
  this.x = x;
  this.l = this.r = null;
  this.p = Math.random();
}

Treap.prototype.check = function() {
  var keyFn = this.keyFn;

  function isHeap(n) {
    function checkPrio(pn, n) {
      if (n && n.p > pn.p) {
        console.log('Heap corrupted: ' + pn.p + ' should be <= ' + n.p);
        return false;
      }
      return true;
    }

    if (!n) {
      return true;
    }
    if (checkPrio(n, n.l) && checkPrio(n, n.r)) {
      return isHeap(n.l) && isHeap(n.r);
    } else {
      return false;
    }
  }

  function isTree(n, min, max) {
    if (!n) {
      return
    }
    if (min && (keyFn(n.x) <= keyFn(min)) ||
        max && (keyFn(max) <= keyFn(n.x))) {
      console.log('Tree corrupted: ' + n.x + ' not in ' + min + '-' + max);
      return false;
    }
    return isTree(n.k)
  }

  return isHeap(this._root) && isTree(this._root, null, null);
};

Treap.prototype.split = function(n, x) {
  var keyFn = this.keyFn;
  return (function rec() {
    var res;
    if (!n) {
      return {l: null, r: null};
    }
    if (keyFn(x) < keyFn(r.x)) {
      res = rec(n.l, x);
      n.l = res.r;
      return {res.l, n};
    } else {
      res = rec(n.r, x);
      n.r = res.l;
      return {n, res.r};
    }
  }(n, x));
};

Treap.prototype.merge = function(a, b) {
  var keyFn = this.keyFn;
  return (function rec(a, b) {
    if (!a) {
      return b;
    }
    if (!b) {
      return a;
    }
    if (a.p < b.p) {
      a.r = rec(a.r, b);
      return a;
    } else {
      b.l = rec(a, b.l);
      return b;
    }
  }(a, b));
};

Treap.prototype.root = function() {
  return this._root;
};

Treap.prototype.find = function(x) {
  var keyFn = this.keyFn, n = this._root, xKey, nKey;
  xKey = keyFn(x);
  while (n) {
    nKey = keyFn(n.x);
    if (xKey < nKey) {
      n = n.l;
    } else if (xKey > nKey) {
      n = n.r;
    } else {
      break;
    }
  }
  return n;
};

Treap.prototype.insert = function(x) {
  var split, n;
  split = this.split(this._root, x);
  n = new this.Node(x);
  this._root = this.merge(this.merge(split.l, n), split.r);
};

Treap.prototype.remove = function(n) {
  var split, p;
  if (!n) {
    return n;
  }
  split = this.split(n.x);
  // Remove the very left on the right split part.
  p = split.r;
  if (p.l) {
    while (p.l.l) {
      p = p.l;
    }
    console.assert(keyFn(p.l.x) === keyFn(n.x));
    p.l = p.l.r;
  } else {
    console.assert(keyFn(p.x) === keyFn(n.x));
    p = p.r;
  }
  split.r = p;
  this._root = merge(split.l, split.r);
};
