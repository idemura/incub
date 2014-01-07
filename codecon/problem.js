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

function ugly(n) {
  return n % 2 === 0 || n % 3 === 0 || n % 5 === 0 || n % 7 === 0;
}

function iter(digits) {
  function rec(s, n, i) {
    var d;
    if (i === digits.length) {
      return ugly(s + n)? 1: 0;
    } else {
      d = digits[i];
      return rec(s + n,  d, i + 1) +
             rec(s + n, -d, i + 1) +
             rec(s, n * 10 + (n < 0? -d: d), i + 1);
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
