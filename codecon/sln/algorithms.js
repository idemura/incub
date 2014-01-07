'use strict';

function print() {
  console.log.apply(null, arguments);
}

function binGCD(a, b) {
  var pot = 1;
  while (a != b) {
    var ae = a & 1, be = b & 1;
    if (ae === 0)
      a >>>= 1;
    if (be === 0)
      b >>>= 1;

    var s = ae + be;
    if (s === 0) {
      pot <<= 1;
    } else if (s === 2) {
      if (a > b) {
        a = (a - b) >>> 1;
      } else {
        b = (b - a) >>> 1;
      }
    }
  }
  return pot * a;
}

// (function() {
//   function check(a, b, ch) {
//     var gcd = binGCD(a, b);
//     if (gcd !== ch) {
//       print('Invalid GCD(' + a + ', ' + b + ') = ' + gcd + ' expected ' + ch);
//     }
//   }
//
//   check(2, 3, 1);
//   check(2, 4, 2);
//   check(12, 4, 4);
//   check(6, 3, 3);
//   check(12, 8, 4);
//   check(21, 15, 3);
//   check(20, 15, 5);
// }());

function quickSort(a) {
  function sortRange(i0, j0) {
    var i = i0, j = j0;
    if (j - i <= 1) {
      return;
    }
    var piv = a[(i + j) >>> 1];
    j--;  // Point to the last item in the array.
    do {
      while (i < j && a[i] < piv) {
        i++;
      }
      while (i < j && a[j] > piv) {
        j--;
      }
      var t = a[i];
      a[i] = a[j];
      a[j] = t;
      i++;  // Do not decrement j! i==j should be classified too.
    } while (i < j);
    if (i == i0) {
      sortRange(i0 + 1, j0);
    } else if (i == j0) {
      sortRange(i0, j0 - 1);
    } else {
      sortRange(i0, i);
      sortRange(i, j0);
    }
  }

  sortRange(0, a.length);
}

// (function() {
//   function check(sortf, a) {
//     sortf(a);
//     var ok = true;
//     for (var i = 1; i < a.length; i++) {
//       if (a[i-1] > a[i]) {
//         print('At ' + i + ': ' + a[i-1] + ' > ' + a[i]);
//         ok = false;
//       }
//     }
//     if (!ok) {
//       print('Test FAILED!');
//     }
//   }
//
//   check(quickSort, []);
//   check(quickSort, [1]);
//   check(quickSort, [1, 2]);
//   check(quickSort, [2, 1]);
//   check(quickSort, [1, 3, 2, 4]);
//   check(quickSort, [4, 3, 2, 1]);
//   check(quickSort, [1, 2, 3, 4]);
//   check(quickSort, [4, 2, 3, 1]);
//   check(quickSort, [4, 2, 3, 1, 5]);
//   check(quickSort, [4, 2, 5, 1, 3]);
//   check(quickSort, [4, 2, 1, 5, 3]);
//   check(quickSort, [4, 2, 5, 1, 3]);
//   check(quickSort, [4, 2, 5, 2, 3]);
//   check(quickSort, [4, 2, 5, 2, 2]);
//   check(quickSort, [2, 2, 2, 2, 2]);
// }());

function getMissing(as) {
  for (var i = 0, n = as.length; i < n; i++) {
    while (-1 < as[i] && as[i] < n && as[i] != i) {
      var t = as[i];
      as[i] = as[t];
      as[t] = t;
    }
  }

  var ms = [];
  for (var i = 0, n = as.length; i < n; i++) {
    if (as[i] != i) {
      ms.push(i);
    }
  }
  return ms;
}

function getMissingRange(as, n) {
  while (as.length < n) {
    as.push(-1);
  }
  return getMissing(as);
}

// (function() {
//   function check(as, n) {
//     print(as);
//     var ms = n? getMissingRange(as, n): getMissing(as);
//     print("  Missing:", ms);
//   }
//
//   check([0, -1, 2]);
//   check([2, -1, 0]);
//   check([4, -1, 3, -1, 0, 5]);
//   // Same as above, but instead -1's we know the range.
//   check([4, 3, 0, 5], 6);
// }());

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

// comb([1, 2, 3, 4], 3, function(set) {
//   print(set);
// });
