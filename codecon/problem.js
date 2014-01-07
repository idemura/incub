'use strict';

function Scanner(text) {
  this.splits = text.split(/\s+/).filter(function(s) { return s.length > 0; });
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

function input(file, callback) {
  var fs = require('fs');
  fs.readFile(file, 'utf8', function(err, data) {
    if (err) {
      process.exit(1);
    }
    // Split on spaces, save that vector. have methods get int, string.
    callback(new Scanner(data));
  });
}

// function iter(digits) {
//   var l = digits.length;
//   function rec(s, i) {
//     var c, d, j;
//     if (i === l) {
//       return s % 2 === 0 || s % 3 === 0 || s % 5 === 0 || s % 7 === 0? 1: 0;
//     } else {
//       d = 0;
//       c = 0;
//       for (j = i; j < l; j++) {
//         d = 10 * d + digits[j];
//         c += rec(s + d, j + 1) + rec(s - d, j + 1);
//       }
//       return c;
//     }
//   }
//   return rec(0, 0) / 2;
// }

function iter(digits) {
  var l = digits.length;
  function rec(s, n, i) {
    var d;
    if (i === l) {
      s += n;
      return s % 2 === 0 || s % 3 === 0 || s % 5 === 0 || s % 7 === 0? 1: 0;
    } else {
      d = digits[i];
      return rec(s + n, d, i + 1) +
             rec(s - n, d, i + 1) +
             rec(s, n * 10 + d, i + 1);
    }
  }
  return rec(0, digits[0], 1);
}

input(process.argv[2], function(sc) {
  var i, s, digits = [];
  while (!sc.eof()) {
    s = sc.getStr();
    digits = [];
    for (i = 0; i < s.length; i++) {
      digits[i] = parseInt(s[i], 10);
    }
    console.log(iter(digits));
  }
});
