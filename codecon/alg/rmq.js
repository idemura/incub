'use strict';

function print() {
  console.log.apply(null, arguments);
}

// Range minimum with sparse table.
function RMQ_ST(as) {
  var st = [];
  st[0] = as.slice();  // Shallow copy.
  for (var l = 1, i = 1; l < as.length; l *= 2, i++) {
    st[i] = [];
    for (var j = 0; j + l < st[i - 1].length; j++) {
      st[i][j] = Math.min(st[i - 1][j], st[i - 1][j + l]);
    }
  }
  this.st = st;
}

RMQ_ST.prototype.print = function() {
  var st = this.st;
  for (var i = 0; i < st.length; i++) {
    var len = 1 << i;
    print('Length ' + len + ':');
    var v = st[i];
    for (var j = 0; j < v.length; j++) {
      print('  Min in ' + st[0].slice(j, j + len) + ' is ' + v[j]);
    }
  }
};

RMQ_ST.prototype.min = function(i, j) {
  var st = this.st;
  // 2 << d = 2 * 2 ^ d.
  for (var d = 0; 2 << d < j - i; d++) {
  }
  return Math.min(st[d][i], st[d][j - (1<<d)]);
};

// (function() {
//   function check(as, rs) {
//     var st = new RMQ_ST(as);
//     // st.print();
//     for (var i = 0; i < rs.length; i++) {
//       var t = rs[i];
//       var m = st.min(t[0], t[1]);
//       // Check min computed straightforward.
//       var mc = 9999;
//       for (var j = t[0]; j < t[1]; j++) {
//         if (as[j] < mc) {
//           mc = as[j];
//         }
//       }
//       if (m != mc) {
//         print('In range ' + as.slice(t[0], t[1]) + ' min is ' + mc +
//               ', but ST ' + m);
//       }
//     }
//   }
//
//   check([2, 1, 4, 3, 6, 5], [[0, 3], [0, 1], [2, 3], [0, 6], [3, 6]]);
//   check([2, 1, 4, 3], [[0, 4], [0, 1], [1, 4], [1, 3]]);
// }());

function SegmentTree(xs) {
  var xsLen, i, m;

  // Return m = 2^k: m >= n.
  function pot(n) {
    for (var m = 1; m < n; m <<= 1) {
    }
    return m;
  }

  // Before building a tree, append Infinity up to power of 2 size.  After we
  // can build balanced tree in array with heap-like indexing.
  // Time: O(n).
  function appendToPOT(xs) {
    var xsLen = xs.length, xsLenPOT = pot(xsLen), i;
    for (i = xsLen; i < xsLenPOT; i++) {
      xs[i] = Infinity;
    }
  }

  appendToPOT(xs);
  xsLen = xs.length;
  m = [];
  for (i = 0; i < xsLen; i++) {
    m[i + xsLen - 1] = xs[i];
  }
  for (i = xsLen - 1; i-- != 0;) {
    m[i] = Math.min(m[2 * i + 1], m[2 * i + 2]);
  }
  this._m = m;
}

SegmentTree.prototype.min = function(i, j) {
  var m = this._m;

  // ni - node index, index in the segment tree _m.
  // ai, aj - is range of the node `ni` (range in the source array).
  function rec(ni, ai, aj, d) {
    var mid, min1, min2;
    // If current segment is outside the request, return Infinity.
    if (aj <= i || ai >= j) {
      return Infinity;
    }
    // If current segment is entirely contained in the query segment [i, j),
    // return the value.
    if (i <= ai && aj <= j) {
      return m[ni];
    } else {
      mid = (ai + aj) >>> 1;
      min1 = rec(2 * ni + 1, ai, mid, d+1);
      min2 = rec(2 * ni + 2, mid, aj, d+1);
      return Math.min(min1, min2);
    }
  }

  // Segment tree has `2*n-1` nodes, where n is the length of source array,
  // filled up to POT.  We maintain range in the source array, while walk down
  // the segment tree.
  var xsLen = (m.length + 1) >>> 1;
  return rec(0, 0, xsLen, i, j, 0);
};

// (function() {
//   var st, xs, i, j;
//
//   function check(i, j) {
//     var expected, k;
//     for (k = i; k < j; k++) {
//       if (expected == undefined || xs[k] < expected) {
//         expected = xs[k];
//       }
//     }
//     var v = st.min(i, j);
//     if (v != expected) {
//       print(util.format('In [%d,%d) min is %d, expected %d', i, j, v, expected));
//     }
//   }
//
//   xs = [1, 6, 2, 9, 3, 4, 5];
//   st = new SegmentTree(xs);
//   for (i = 0; i < xs.length; i++) {
//     for (j = i + 1; j < xs.length; j++) {
//       check(i, j);
//     }
//   }
//   print('DONE.');
// }());
